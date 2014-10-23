#! /usr/bin/env runhaskell
{-# LANGUAGE LambdaCase, RecordWildCards, NamedFieldPuns #-}
module Main where


import           Control.Applicative
import           Control.Monad
import           Data.List
import           Safe
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           Whacked.Tree
import           Whacked.Itch
import           Whacked.Scratch
import           Whacked.Frontend.Parser
import           Whacked.Frontend.Generator
--import           Whacked.Optimizer.Translator
--import           Whacked.Optimizer.SCCP
--import           Whacked.Backend.ARM as ARM


data Options
  = Options
    { optPrintAST :: Bool
    , optPrintIMF :: Bool
    , optPrintHelp :: Bool
    , optPrintASM :: Bool
    , optOutput :: String
    }
  deriving (Eq, Ord, Show)


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
        (OptArg
          (\val opt -> case val of
            Nothing -> opt
            Just val -> opt{ optOutput = val })
          (optOutput defaultOptions))
        "Choose the output file"
    ]


defaultOptions :: Options
defaultOptions
  = Options
    { optPrintAST = False
    , optPrintIMF = False
    , optPrintHelp = False
    , optPrintASM = False
    , optOutput = "wacc.s"
    }


usage :: IO ()
usage = do
  prog <- getProgName
  let header = "The Whacked Compiler\n\n" ++
               "Usage:\n" ++
               "  " ++ prog ++ " [args] source\n"
  putStrLn $ usageInfo header options


main :: IO ()
main
  = getOpt Permute options <$> getArgs >>= \case
    (opts, [sourceName], []) -> do
      let Options{..} = foldl (flip ($)) defaultOptions opts

      -- |Print usage information and quit if requested.
      when optPrintHelp (usage >> exitSuccess)

      -- |Quit if file does not exist.
      doesFileExist sourceName >>= \exists -> unless exists $ do
        putStrLn $ "[" ++ sourceName ++ "]: File does not exist."
        exitFailure

      -- |Get the AST.
      readFile sourceName >>= \source -> case parse sourceName source of
        Left err -> do
          putStrLn (show err)
          exitWith $ ExitFailure 100
        Right ast -> do
          when optPrintAST $ print ast
          case generateI ast of
            Left err -> do
              putStrLn err
              exitWith $ ExitFailure 200
            Right itch -> do
              when optPrintIMF $ do
                forM_ (ipFuncs itch) $ \IFunction{..} -> do
                  putStrLn (ifName ++ show ifArgs ++ show ifVars)
                  mapM_ (putStrLn . show) ifBody

              {-let scratch = sccp . generateS $ itch
              when optPrintIMF $ do
                forM_ (spFuncs scratch) $ \SFunction{..} -> do
                  putStrLn (show sfArgs)
                  mapM_ (putStrLn . show) sfBody

              let asm = ARM.compile scratch
              when optPrintASM $ do
                  mapM_ (putStrLn . show) asm

              writeFile optOutput (concat . intersperse "\n" . map show $ asm)-}

    (_, _, errs) -> usage