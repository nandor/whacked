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
import           Whacked.Tree
import           Whacked.Types


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
      , Token.reservedOpNames =
          [ "="
          , "&&"
          , "||"
          ]
      , Token.reservedNames   =
          [ "begin"
          , "end"
          , "int"
          , "is"
          , "if"
          , "then"
          , "else"
          , "fi"
          , "elif"
          , "return"
          , "while"
          , "do"
          , "done"
          , "void"
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


aType :: GenParser Char st Type
aType
  = asum
    [ reserved "int" *> return Int
    , reserved "bool" *> return Bool
    , reserved "void" *> return Void
    ]


aLValue :: GenParser Char st ALValue
aLValue
  = asum $ map try
    [ ALVar <$> aTag <*> identifier
    ]


aRValue :: GenParser Char st AExpr
aRValue = do
  tag <- aTag
  asum $ map try
    [ parens aExpr
    , ACall tag <$> identifier <*> parens (commaSep aExpr)
    , AVar tag <$> identifier
    , AConstInt tag . fromIntegral <$> natural
    ]


aExprOp :: OperatorTable Char st AExpr
aExprOp
  = [ [ Infix (tagBin "*"  Mul) AssocLeft
      , Infix (tagBin "/"  Div) AssocLeft
      , Infix (tagBin "%"  Mod) AssocLeft
      ]
    , [ Infix (tagBin "+"  Add) AssocLeft
      , Infix (tagBin "-"  Sub) AssocLeft
      ]
    , [ Infix (tagBin "<" CmpLt) AssocNone
      , Infix (tagBin ">" CmpLt) AssocNone
      ]
    , [ Infix (tagBin "&&" And) AssocRight
      ]
    , [ Infix (tagBin "||" Or) AssocRight
      ]
    ]
  where
    tagBin op name = do
      tag <- aTag
      reservedOp op
      return $ \lhs rhs -> ABinOp tag name lhs rhs


aExpr :: GenParser Char u AExpr
aExpr
  = buildExpressionParser aExprOp aRValue


aStatement :: GenParser Char st AStatement
aStatement
  = asum $ map try
    [ aReturn
    , aPrint
    , aAssign
    , aVarDecl
    , aWhile
    , aBlock
    , aIf
    ]
  where
    aReturn = do
      tag <- aTag
      reserved "return"
      expr <- aExpr
      return $ AReturn tag expr

    aPrint = do
      tag <- aTag
      reserved "print"
      expr <- aExpr
      return $ APrint tag expr

    aAssign = do
      tag <- aTag
      lval <- aLValue
      reservedOp "="
      expr <- aExpr
      return $ AAssign tag lval expr

    aVarDecl = do
      tag <- aTag
      varType <- aType
      vars <- commaSep1 $ do
        tag <- aTag
        name <- identifier
        val <- optionMaybe (reservedOp "=" *> aExpr)
        return (tag, name, val)
      return $ AVarDecl tag varType vars

    aWhile = do
      tag <- aTag
      reserved "while"
      cond <- aExpr
      reserved "do"
      body <- semiSep1 aStatement
      reserved "done"
      return $ AWhile tag cond body

    aBlock = do
      tag <- aTag
      reserved "begin"
      body <- semiSep1 aStatement
      reserved "end"
      return $ ABlock tag body

    aIf = do
      tag <- aTag
      reserved "if"
      cond <- aExpr
      reserved "then"
      true <- semiSep1 aStatement
      asum
        [ reserved "fi" *> return (AIf tag cond true [])
        , reserved "else" *> asum
          [ try $ do
              false <- aIf
              return $ AIf tag cond true [false]
          , try $ do
              false <- semiSep aStatement
              reserved "fi"
              return $ AIf tag cond true false
          ]
        ]


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
  return $ AFunction tag args retType name body


aProgram :: GenParser Char st AProgram
aProgram = do
  reserved "begin"
  functions <- many (try aFunction)
  cons <- AFunction <$> aTag
  body <- semiSep aStatement
  reserved "end"
  return $ AProgram (cons [] Int "main" body : functions)


parse :: String -> String -> Either ParseError AProgram
parse sourceName source
 = Parsec.parse (whiteSpace *> aProgram <* eof) sourceName source
