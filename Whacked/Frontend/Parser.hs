{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Whacked.Frontend.Parser
  ( parse
  , ParseError
  ) where


import           Control.Applicative ((*>), (<*), (<$>), (<*>))
import           Data.Foldable
import           Data.Functor.Identity
import           Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Pos
import           Text.ParserCombinators.Parsec.Token (TokenParser)
import qualified Text.ParserCombinators.Parsec.Token as Token
import           Whacked.AST


-- |Definition of the language.
whacked :: TokenParser st
whacked
  = Token.makeTokenParser whacked'
  where
    whacked' = Token.LanguageDef
      { Token.commentStart    = ""
      , Token.commentEnd      = ""
      , Token.commentLine     = "#"
      , Token.nestedComments  = False
      , Token.identStart      = letter <|> char '_'
      , Token.identLetter     = alphaNum <|> char '_'
      , Token.opStart         = Token.opLetter whacked'
      , Token.opLetter        = oneOf "!%&*+./<=>?^|-~"
      , Token.reservedOpNames = []
      , Token.reservedNames   =
          [ "begin"
          , "end"
          , "int"
          , "is"
          , "return"
          ]
      , Token.caseSensitive   = True
      }


-- |Basic, common tokens.
identifier       = Token.identifier whacked
reserved         = Token.reserved whacked
operator         = Token.operator whacked
reservedOp       = Token.reservedOp whacked
charLiteral      = Token.charLiteral whacked
stringLiteral    = Token.stringLiteral whacked
natural          = Token.natural whacked
integer          = Token.integer whacked
float            = Token.float whacked
naturalOrFloat   = Token.naturalOrFloat whacked
decimal          = Token.decimal whacked
hexadecimal      = Token.hexadecimal whacked
octal            = Token.octal whacked
symbol           = Token.symbol whacked
lexeme           = Token.lexeme whacked
whiteSpace       = Token.whiteSpace whacked
parens           = Token.parens whacked
braces           = Token.braces whacked
angles           = Token.angles whacked
brackets         = Token.brackets whacked
squares          = Token.squares whacked
semi             = Token.semi whacked
comma            = Token.comma whacked
colon            = Token.colon whacked
dot              = Token.dot whacked
semiSep          = Token.semiSep whacked
semiSep1         = Token.semiSep1 whacked
commaSep         = Token.commaSep whacked
commaSep1        = Token.commaSep1 whacked


aTag :: GenParser Char st ATag
aTag = do
  pos <- getPosition
  return $ ATag (sourceName pos) (sourceLine pos) (sourceColumn pos)


aType :: GenParser Char st AType
aType
  = asum
    [ reserved "int" *> return AInt
    , reserved "bool" *> return ABool
    ]


aRValue :: GenParser Char st AExpr
aRValue = do
  tag <- aTag
  term <- asum $ map try
    [ parens aExpr
    , ACall tag <$> identifier <*> parens (commaSep aExpr)
    , AVar tag <$> identifier
    , AConstInt tag . fromIntegral <$> natural
    ]
  return term


aExprOp :: OperatorTable Char st AExpr
aExprOp
  = [ [ Infix  (tagBinApp "*"  AMul) AssocLeft
      , Infix  (tagBinApp "/"  ADiv) AssocLeft
      , Infix  (tagBinApp "%"  AMod) AssocLeft
      ]
    , [ Infix  (tagBinApp "+"  AAdd) AssocLeft
      , Infix  (tagBinApp "-"  ASub) AssocLeft
      ]
    ]
  where
    tagBinApp :: String -> ABinary -> GenParser Char u (AExpr -> AExpr -> AExpr)
    tagBinApp op name = do
      tag <- aTag
      reservedOp op
      return $ \lhs rhs -> ABinOp tag name lhs rhs


aExpr :: GenParser Char u AExpr
aExpr
  = buildExpressionParser aExprOp aRValue


aStatement :: GenParser Char st AStatement
aStatement
  = asum
    [ aReturn
    , aPrint
    ]
  where
    aReturn :: GenParser Char st AStatement
    aReturn = do
      tag <- aTag
      reserved "return"
      expr <- aExpr
      return $ AReturn tag expr


    aPrint :: GenParser Char st AStatement
    aPrint = do
      tag <- aTag
      reserved "print"
      expr <- aExpr
      return $ APrint tag expr


aFunction :: GenParser Char st AFunction
aFunction = do
  tag <- aTag
  retType <- aType
  name <- identifier
  args <- parens . commaSep $ do
    tag <- aTag
    argType <- aType
    argName <- identifier
    return $ AArg tag argType argName
  reserved "is"
  body <- semiSep aStatement
  reserved "end"
  return $ AFunction args retType name [] tag


aProgram :: GenParser Char st AProgram
aProgram = do
  reserved "begin"
  functions <- many aFunction
  body <- semiSep aStatement
  reserved "end"
  return $ AProgram functions body


parse :: String -> String -> Either ParseError AProgram
parse sourceName source
 = Parsec.parse (whiteSpace *> aProgram <* eof) sourceName source
