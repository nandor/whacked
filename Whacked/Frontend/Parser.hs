{-# LANGUAGE RecordWildCards, NamedFieldPuns, LambdaCase #-}
module Whacked.Frontend.Parser
  ( parse
  , ParseError
  ) where


import           Control.Applicative ((*>), (<*), (<$>), (<*>), pure)
import           Control.Monad
import           Data.Foldable hiding (foldl)
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
import           Whacked.FlowGraph



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
          , "len"
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
      , reserved "char"   *> return Char
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
      count <- length <$> many1 (brackets whiteSpace)
      return $ foldl (\x _ -> Array x) t [1..count]


-- |Parses a LValue.
aLValue :: GenParser Char st ALValue
aLValue
  = asum . map try $
    [ do
        tag <- aTag
        expr <- aExpr
        case expr of
          AIndex{..} ->
            return $ ALArray tag aeArray aeIndex
          _ ->
            unexpected "expected an array index"
    , ALVar   <$> aTag <*> identifier
    , ALElem  <$> aTag <*> (reserved "fst" *> aExpr) <*> pure Fst
    , ALElem  <$> aTag <*> (reserved "snd" *> aExpr) <*> pure Snd
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
    , ARElem <$> aTag <*> (reserved "fst" *> aExpr) <*> pure Fst
    , ARElem <$> aTag <*> (reserved "snd" *> aExpr) <*> pure Snd
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
        , [ Prefix (tagUn (reservedOp "!") Not)
          , Prefix (tagUn (reservedOp "-") Neg)
          , Prefix (tagUn (reserved "len") Len)
          , Prefix (tagUn (reserved "ord") Ord)
          , Prefix (tagUn (reserved "chr") Chr)
          ]
        , [ Infix (tagBin "*" Mul) AssocLeft
          , Infix (tagBin "/" Div) AssocLeft
          , Infix (tagBin "%" Mod) AssocLeft
          ]
        , [ Infix (tagBin "+" Add) AssocLeft
          , Infix (tagBin "-" Sub) AssocLeft
          ]
        , [ Infix (tagBin ">"  $ Cmp CGT ) AssocNone
          , Infix (tagBin ">=" $ Cmp CGE) AssocNone
          , Infix (tagBin "<"  $ Cmp CLT ) AssocNone
          , Infix (tagBin "<=" $ Cmp CLE) AssocNone
          ]
        , [ Infix (tagBin "==" $ Cmp CEQ) AssocNone
          , Infix (tagBin "!=" $ Cmp CNE) AssocNone
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

    tagUn matcher name = do
      tag <- aTag
      matcher
      return $ \arg -> AUnOp tag name arg

    atom = asum . map try $
      [ do
          tag <- aTag
          int <- integer
          when (1 + toInteger (maxBound :: Int32) < int) $
            unexpected $ "integer too big: " ++ show int
          return $ AInt tag (fromIntegral int)
      , ABool <$> aTag <*> asum
        [ reserved "true" *> return True
        , reserved "false" *> return False
        ]
      , do
          tag <- aTag
          char '\''
          chr <- noneOf "\""
          if chr /= '\\'
            then lexeme (char '\'') *> return (AChar tag chr)
            else do
              chr <- anyChar
              lexeme $ char '\''
              AChar tag <$> case chr of
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
      , AString <$> aTag <*> stringLiteral
      , AVar <$> aTag <*> identifier
      , reserved "null" *> (ANull <$> aTag)
      , parens aExpr
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
  -- Vi Coactus, I would have made this a semantic error.
  unless (allPathsReturn body) $ unexpected "not all control paths return"
  return $ AFunction tag args retType name body


-- |Parses an entire program.
aProgram :: GenParser Char st AProgram
aProgram = do
  tag <- aTag
  reserved "begin"
  functions <- many (try aFunction)
  body <- semiSep1 aStatement
  reserved "end"
  return $ AProgram (AFunction tag [] Void "main" (body ++ [AEnd]) : functions)


-- |Runs the parser and return the AST or an error.
parse :: String -> String -> Either ParseError AProgram
parse
 = Parsec.parse (whiteSpace *> aProgram <* eof)
