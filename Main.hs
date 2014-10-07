{-# LANGUAGE LambdaCase, RecordWildCards, NamedFieldPuns #-}
module Main where

import Control.Applicative
import Control.Monad
import Safe
import System.Console.GetOpt
import System.Environment
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
    (opts, [source], []) -> do
      let Options{..} = foldl (flip ($)) defaultOptions opts
      when optPrintHelp usage
      print source
    (_, _, errs) -> usage