{-# LANGUAGE RecordWildCards, NamedFieldPuns, LambdaCase #-}
module Whacked.Frontend.Parser
  ( parse
  , ParseError
  ) where


import           Control.Applicative ((*>), (<*), (<$>), (<*>))
import           Control.Monad
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Int
import           Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Pos
import           Text.ParserCombinators.Parsec.Token (TokenParser)
import qualified Text.ParserCombinators.Parsec.Token as Token
import           Whacked.Tree
import           Whacked.Types

import Debug.Trace

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
          , "ord"
          , "chr"
          , "null"
          , "free"
          , "newpair"
          ]
      , Token.caseSensitive   = True
      }


-- |Basic, common tokens.
identifier    = Token.identifier whacked
reserved      = Token.reserved whacked
operator      = Token.operator whacked
reservedOp    = Token.reservedOp whacked
charLiteral   = Token.charLiteral whacked
stringLiteral = Token.stringLiteral whacked
integer       = Token.integer whacked
lexeme        = Token.lexeme whacked
whiteSpace    = Token.whiteSpace whacked
parens        = Token.parens whacked
brackets      = Token.brackets whacked
squares       = Token.squares whacked
comma         = Token.comma whacked
semiSep1      = Token.semiSep1 whacked
commaSep      = Token.commaSep whacked


-- |Returns the position of a token in the source file.
aTag :: GenParser Char st ATag
aTag = do
  pos <- getPosition
  return $ ATag (sourceName pos) (sourceLine pos) (sourceColumn pos)


-- |Parses a type declaration.
aType :: GenParser Char st Type
aType
  = asum . map try $
    [ arrayType
    , baseType
    , pairType
    ]
  where
    baseType = asum . map try $
      [ reserved "int"    *> return Int
      , reserved "bool"   *> return Bool
      , reserved "string" *> return String
      , reserved "char"   *> return (Array Char 1)
      ]

    pairElemType = asum . map try $
      [ arrayType
      , baseType
      , reserved "pair" *> return Poly
      ]

    pairType = do
      reserved "pair"
      parens $ do
        left <- pairElemType
        comma
        right <- pairElemType
        return $ Pair left right

    arrayType = do
      t <- baseType <|> pairType
      count <- length <$> (many1 (brackets whiteSpace))
      return $ Array t count


-- |Parses a LValue.
aLValue :: GenParser Char st ALValue
aLValue
  = asum . map try $
    [ ALArray <$> aTag <*> identifier <*> brackets aExpr
    , ALVar   <$> aTag <*> identifier
    , ALFst   <$> aTag <*> (reserved "fst" *> aExpr)
    , ALSnd   <$> aTag <*> (reserved "snd" *> aExpr)
    ]


-- |Parses a RValue.
aRValue :: GenParser Char st ARValue
aRValue
  = asum $ map try
    [ ARExpr  <$> aTag <*> aExpr
    , ARArray <$> aTag <*> brackets (commaSep aExpr)
    , do
        tag <- aTag
        reserved "newpair"
        parens $ do
          left <- aExpr
          comma
          right <- aExpr
          return $ ARPair tag left right
    , ARFst   <$> aTag <*> (reserved "fst" *> aExpr)
    , ARSnd   <$> aTag <*> (reserved "snd" *> aExpr)
    , do
        tag <- aTag
        reserved "call"
        func <- identifier
        args <- parens (commaSep aExpr)
        return $ ARCall tag func args
    ]


-- |Parses an expression.
aExpr :: GenParser Char u AExpr
aExpr
  = buildExpressionParser operatorTable atom
  where
    operatorTable
      = [ [ Postfix  (chainl1 (try index) $ return (flip (.)))
          ]
        , [ Prefix (tagUn "!"   Not)
          , Prefix (tagUn "-"   Neg)
          , Prefix (tagUn "len" Len)
          , Prefix (tagUn "ord" Ord)
          , Prefix (tagUn "chr" Chr)
          ]
        , [ Infix (tagBin "*" Mul) AssocLeft
          , Infix (tagBin "/" Div) AssocLeft
          , Infix (tagBin "%" Mod) AssocLeft
          ]
        , [ Infix (tagBin "+" Add) AssocLeft
          , Infix (tagBin "-" Sub) AssocLeft
          ]
        , [ Infix (tagBin ">"  $ Cmp CGT ) AssocNone
          , Infix (tagBin ">=" $ Cmp CGTE) AssocNone
          , Infix (tagBin "<"  $ Cmp CLT ) AssocNone
          , Infix (tagBin "<=" $ Cmp CLTE) AssocNone
          ]
        , [ Infix (tagBin "==" $ Cmp CEQ ) AssocNone
          , Infix (tagBin "!=" $ Cmp CNEQ) AssocNone
          ]
        , [ Infix (tagBin "&&" And) AssocRight
          ]
        , [ Infix (tagBin "||" Or) AssocRight
          ]
        ]

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

    atom = asum . map try $
      [ parens aExpr
      , do
          tag <- aTag
          int <- integer
          when (1 + toInteger (maxBound :: Int32) < int) $
            unexpected $ "integer too big: " ++ show int
          return $ AConstInt tag (fromIntegral int)
      , AConstBool <$> aTag <*> asum
        [ reserved "true" *> return True
        , reserved "false" *> return False
        ]
      , do
          tag <- aTag
          char '\''
          chr <- noneOf "\""
          if chr /= '\\'
            then lexeme (char '\'') *> return (AConstChar tag chr)
            else do
              chr <- anyChar
              lexeme $ char '\''
              AConstChar tag <$> case chr of
                '0'  -> return '\0'
                'b'  -> return '\b'
                't'  -> return '\t'
                'n'  -> return '\n'
                'f'  -> return '\f'
                'r'  -> return '\r'
                '"'  -> return '\"'
                '\'' -> return '\''
                '\\' -> return '\\'
                x -> unexpected "invalid escape sequence"
      , AConstString <$> aTag <*> stringLiteral
      , AVar <$> aTag <*> identifier
      , reserved "null" *> (ANull <$> aTag)
      ]


-- |Parses a statement.
aStatement :: GenParser Char st AStatement
aStatement
  = asum . map try $
    [ ASkip    <$> (aTag <* reserved "skip")
    , AVarDecl <$> aTag <*> aType <*> identifier <*> (reservedOp "=" *> aRValue)
    , AAssign  <$> aTag <*> aLValue <*> (reservedOp "=" *> aRValue)
    , ARead    <$> aTag <*> (reserved "read"    *> aLValue)
    , AFree    <$> aTag <*> (reserved "free"    *> aExpr)
    , AReturn  <$> aTag <*> (reserved "return"  *> aExpr)
    , AExit    <$> aTag <*> (reserved "exit"    *> aExpr)
    , APrint   <$> aTag <*> (reserved "print"   *> aExpr)
    , APrintln <$> aTag <*> (reserved "println" *> aExpr)
    , do
        tag <- aTag
        reserved "if"
        cond <- aExpr
        reserved "then"
        true <- semiSep1 aStatement
        reserved "else"
        false <- semiSep1 aStatement
        reserved "fi"
        return $ AIf tag cond true false
    , do
        tag <- aTag
        reserved "while"
        cond <- aExpr
        reserved "do"
        body <- semiSep1 aStatement
        reserved "done"
        return $ AWhile tag cond body
    , do
        tag <- aTag
        reserved "begin"
        body <- semiSep1 aStatement
        reserved "end"
        return $ ABlock tag body
    ]


-- |Parses a function definition.
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
  body <- semiSep1 aStatement
  reserved "end"
  return $ AFunction tag args retType name body


-- |Parses an entire program.
aProgram :: GenParser Char st AProgram
aProgram = do
  tag <- aTag
  reserved "begin"
  functions <- many (try aFunction)
  body <- (++ [AEnd]) <$> semiSep1 aStatement
  reserved "end"
  return $ AProgram (AFunction tag [] Void "main" body : functions)


-- |Runs the parser and return the AST or an error.
parse :: String -> String -> Either ParseError AProgram
parse sourceName source
 = Parsec.parse (whiteSpace *> aProgram <* eof) sourceName source
