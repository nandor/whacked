#! /usr/bin/env runhaskell
{-# LANGUAGE LambdaCase, RecordWildCards, NamedFieldPuns #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           Whacked.Tree
import           Whacked.Itch
import           Whacked.Scratch
import           Whacked.FlowGraph
import           Whacked.Frontend.Parser
import           Whacked.Frontend.Generator
import           Whacked.Optimizer.Simplifier
import           Whacked.Optimizer.RemovePHI
import           Whacked.Optimizer.Translator
import           Whacked.Optimizer.SCCP
import           Whacked.Backend.ARM as ARM



-- |Structure to hold command line options.
data Options
  = Options
    { optPrintAST :: Bool
    , optPrintIMF :: Bool
    , optPrintHelp :: Bool
    , optPrintASM :: Bool
    , optOutput :: Maybe String
    }
  deriving (Eq, Ord, Show)


-- |List of command line options parsed by getopt.
options :: [ OptDescr (Options -> Options) ]
options
  = [ Option "A" ["print-ast"]
        (NoArg $ \opt -> opt{ optPrintAST = True })
        "Print the abstract syntax tree"
    , Option "I" ["print-imf"]
        (NoArg $ \opt -> opt{ optPrintIMF = True })
        "Print the intermediate code"
    , Option "P" ["print-asm"]
        (NoArg $ \opt -> opt{ optPrintASM = True })
        "Print the generated assembly"
    , Option "h" ["help"]
        (NoArg $ \opt -> opt{ optPrintHelp = True })
        "Print the help message"
    , Option "o" ["output"]
        (ReqArg (\val opt -> opt{ optOutput = Just val }) "FILE")
        "Choose the output file"
    ]


-- |Default options.
defaultOptions :: Options
defaultOptions
  = Options
    { optPrintAST = False
    , optPrintIMF = False
    , optPrintHelp = False
    , optPrintASM = False
    , optOutput = Nothing
    }


-- |Prints help.
usage :: IO ()
usage = do
  prog <- getProgName
  let header = "The Whacked Compiler\n\n" ++
               "Usage:\n" ++
               "  " ++ prog ++ " [args] source\n"
  putStrLn $ usageInfo header options


-- |Entry point of the compiler. Parses arguments & starts the pipeline.
main :: IO ()
main
  = getOpt Permute options <$> getArgs >>= \case
    -- Compile stuff if argument parsing succeeds.
    (opts, [sourceName], []) -> do
      let Options{..} = foldl (flip ($)) defaultOptions opts

      -- |Print usage information and quit if requested.
      when optPrintHelp (usage >> exitSuccess)

      -- |Quit if file does not exist.
      doesFileExist sourceName >>= \exists -> unless exists $ do
        putStrLn $ "[" ++ sourceName ++ "]: File does not exist."
        exitFailure

      -- |Parse, optimize and compile stuff.
      readFile sourceName >>= \source -> case parse sourceName source of
        Left err -> do
          print err
          exitWith $ ExitFailure 100
        Right ast -> case generateI ast of
          Left err -> do
            putStrLn err
            exitWith $ ExitFailure 200
          Right itch -> do
            let -- Optimized intermediate form.
                scratch = pruneCallGraph        -- Removes unused functions.
                        . mapF ( pruneFlowGraph -- Removes unreachable code.
                               . flatten        -- Replaces basic blocks.
                               . simplify       -- Replaces IMF with core calls.
                               . removePhi      -- Removes PHI nodes.
                               . moveConstants  -- Computes constants.
                               . sccp           -- Propagates constants.
                               )
                        . generateS             -- Generate Scratchy code.
                        $ itch

                -- Assembly code.
                asm = ARM.compile scratch

                -- Output file name.
                (file, _) = splitExtension sourceName
                out = fromMaybe (file ++ ".s") optOutput

            when optPrintAST $ print ast
            when optPrintIMF $ print itch
            when optPrintIMF $ print scratch
            when optPrintASM $ mapM_ print asm
            writeFile out $ concatMap (\x -> show x ++ "\n") asm

    -- Display an error message if argument parsing fails.
    (_, _, errs) -> usage