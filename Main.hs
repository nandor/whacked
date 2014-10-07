{-# LANGUAGE LambdaCase, RecordWildCards, NamedFieldPuns #-}
module Main where

import Control.Applicative
import Control.Monad
import Safe
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import Whacked.Frontend.Parser
import Whacked.Frontend.Generator

data Options
  = Options
    { optPrintAST :: Bool
    , optPrintIMF :: Bool
    , optPrintHelp :: Bool
    , optOptimise :: Int
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
    , Option "h" ["help"]
        (NoArg $ \opt -> opt{ optPrintHelp = True })
        "Print the help message"
    , Option "O" ["optimize"]
        (OptArg (\val opt -> case val >>= readMay of
          Nothing -> opt{ optOptimise = 1 }
          Just val -> opt{ optOptimise = val }) "1")
        "Choose an optimisation level"
    ]


defaultOptions :: Options
defaultOptions
  = Options
    { optPrintAST = False
    , optPrintIMF = False
    , optPrintHelp = False
    , optOptimise = 1
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
    (opts, [sourcePath], []) -> do
      let Options{..} = foldl (flip ($)) defaultOptions opts

      -- |Print usage information and quit if requested.
      when optPrintHelp (usage >> exitSuccess)

      -- |Quit if file does not exist.
      doesFileExist sourcePath >>= \exists -> unless exists $ do
        putStrLn $ "[" ++ sourcePath ++ "]: File does not exist."
        exitFailure

      -- |Get the AST.
      source <- readFile sourcePath
      case parse source of
        Left err -> print err
        Right ast -> do
          when optPrintAST $ print ast

          -- |TODO: The rest.

    (_, _, errs) -> usage