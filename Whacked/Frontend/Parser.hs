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
          , "=="
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
          , "read"
          , "print"
          , "println"
          , "call"
          , "exit"
          , "fst"
          , "snd"
          , "toInt"
          , "null"
          , "free"
          , "newpair"
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


aBaseType :: GenParser Char st Type
aBaseType
  = asum . map try $
    [ reserved "int"    *> return Int
    , reserved "bool"   *> return Bool
    , reserved "string" *> return String
    , reserved "char"   *> return Char
    , reserved "real"   *> return Real
    , reserved "void"   *> return Void
    ]


aPairElemType :: GenParser Char st Type
aPairElemType
  = asum . map try $
    [ aArrayType
    , aBaseType
    , reserved "pair" *> return Poly
    ]


aPairType :: GenParser Char st Type
aPairType = do
  (l, r) <- reserved "pair" *> parens (do
    l <- aPairElemType
    comma
    r <- aPairElemType
    return (l, r))
  return $ Pair l r


aArrayType :: GenParser Char st Type
aArrayType = do
  t <- aBaseType <|> aPairType
  count <- length <$> (many1 (brackets whiteSpace))
  return $ Array t count


aType :: GenParser Char st Type
aType
  = asum . map try $
    [ aArrayType
    , aBaseType
    , aPairType
    ]


aLValue :: GenParser Char st ALValue
aLValue
  = asum . map try $
    [ ALArray <$> aTag <*> identifier <*> brackets aExpr
    , ALVar <$> aTag <*> identifier
    , ALFst <$> aTag <*> (reserved "fst" *> identifier)
    , ALSnd <$> aTag <*> (reserved "snd" *> identifier)
    ]


aRValue :: GenParser Char st ARValue
aRValue
  = asum $ map try
    [ ARExpr <$> aTag <*> aExpr
    , ARArray <$> aTag <*> brackets (commaSep aExpr)
    ]


aExprAtom :: GenParser Char st AExpr
aExprAtom = do
  tag <- aTag
  asum . map try $
    [ parens aExpr
    , do
        reserved "newpair"
        (l, r) <- parens $ do
          l <- aExpr
          comma
          r <- aExpr
          return (l, r)
        return $ ANewPair tag l r
    , do
        reserved "call"
        name <- identifier
        args <- parens (commaSep aExpr)
        return $ ACall tag name args
    , AConstInt tag . fromIntegral <$> integer
    , AConstString tag <$> stringLiteral
    , AConstChar tag <$> charLiteral
    , AConstBool tag <$> asum
      [ reserved "true" *> return True
      , reserved "false" *> return False
      ]
    , AVar tag <$> identifier
    , reserved "null" *> return (ANull tag)
    ]


aExprOp :: OperatorTable Char st AExpr
aExprOp
  = [ [ Postfix  (chainl1 (try index) $ return (flip (.)))
      ]
    , [ Prefix (tagUn "!" Not)
      , Prefix (tagUn "-" Neg)
      , Prefix (tagUn "toInt" ToInt)
      , Prefix (tagUn "ord" Ord)
      , Prefix (tagUn "fst" Fst)
      , Prefix (tagUn "snd" Snd)
      , Prefix (tagUn "len" Len)
      ]
    , [ Infix (tagBin "*" Mul) AssocLeft
      , Infix (tagBin "/" Div) AssocLeft
      , Infix (tagBin "%" Mod) AssocLeft
      ]
    , [ Infix (tagBin "+" Add) AssocLeft
      , Infix (tagBin "-" Sub) AssocLeft
      ]
    , [ Infix (tagBin "<" $ Cmp CLT) AssocNone
      , Infix (tagBin ">" $ Cmp CGT) AssocNone
      , Infix (tagBin "==" $ Cmp CEQ) AssocNone
      , Infix (tagBin "!=" $ Cmp CNEQ) AssocNone
      , Infix (tagBin "<=" $ Cmp CLTE) AssocNone
      , Infix (tagBin ">=" $ Cmp CGTE) AssocNone
      ]
    , [ Infix (tagBin "&&" And) AssocRight
      ]
    , [ Infix (tagBin "||" Or) AssocRight
      ]
    ]
  where
    index = do
      tag <- aTag
      expr <- brackets aExpr
      return $ \t -> AIndex tag t expr
    tagBin op name = do
      tag <- aTag
      reservedOp op
      return $ \lhs rhs -> ABinOp tag name lhs rhs
    tagUn op name = do
      tag <- aTag
      reservedOp op
      return $ \arg -> AUnOp tag name arg


aExpr :: GenParser Char u AExpr
aExpr
  = buildExpressionParser aExprOp aExprAtom


aStatement :: GenParser Char st AStatement
aStatement
  = asum $ map try
    [ aVarDecl
    , aAssign
    , aWhile
    , aBlock
    , aIf
    , ASkip    <$> (aTag <* reserved "skip")
    , ARead    <$> aTag <*> (reserved "read" *> aLValue)
    , AExit    <$> aTag <*> (reserved "exit" *> aExpr)
    , APrint   <$> aTag <*> (reserved "print" *> aExpr)
    , APrintln <$> aTag <*> (reserved "println" *> aExpr)
    , AReturn  <$> aTag <*> (reserved "return" *> aExpr)
    , AFree    <$> aTag <*> (reserved "free" *> aExpr)
    ]
  where
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
        val <- reservedOp "=" *> aRValue
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
      true <- semiSep aStatement
      reserved "else"
      false <- semiSep aStatement
      reserved "fi"
      return $ AIf tag cond true false


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
  tag <- aTag
  reserved "begin"
  functions <- many (try aFunction)
  body <- semiSep1 aStatement
  reserved "end"
  return $ AProgram (AFunction tag [] Void "main" body : functions)


parse :: String -> String -> Either ParseError AProgram
parse sourceName source
 = Parsec.parse (whiteSpace *> aProgram <* eof) sourceName source
