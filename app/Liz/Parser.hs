{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE RecordWildCards #-}

module Liz.Parser where

import qualified Liz.Common.Errors as E
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Liz.Common.Types

import Data.String (IsString (..))
import Data.Char (isAlphaNum, isDigit, isPrint) 
import Control.Monad (void, liftM)
import Data.List (find)
import Unsafe.Coerce (unsafeCoerce)
import Data.Foldable (fold)

import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

default IsString (T.Text)
type Parser = Parsec E.PError T.Text

-- helper error functions
failedTypeInference :: T.Text -> Parser a
failedTypeInference = customFailure . E.FailedTypeInference

reservedIdent :: T.Text -> Parser a
reservedIdent = customFailure . E.ReservedIdent

unsupportedDeclaration :: T.Text -> Parser a
unsupportedDeclaration = customFailure . E.UnsupportedDeclaration

invalidNumber :: T.Text -> Parser a
invalidNumber = customFailure . E.InvalidNumber

tooManyExprsIf :: Parser a
tooManyExprsIf = customFailure E.TooManyExprsIf

wrongArgCount :: Int -> Int -> Parser a
wrongArgCount ex got = customFailure $ E.WrongArgCount ex got

-- helper parsing functions
getCurrentLine :: Parser Int
getCurrentLine = getSourcePos >>= pure . unsafeCoerce . sourceLine

head' :: [a] -> a
head' = NE.head . NE.fromList

last' :: [a] -> a
last' = NE.last . NE.fromList

scn :: Parser ()
scn = L.space space1 (void $ spaceChar <|> tab) empty

lizReserved :: [T.Text]
lizReserved = 
  [ "var", "set", "const", "if", "def", "return", "False",
    "True", "not", "negate", "Int", "Float", "String", 
    "Char", "Bool", "Unit", "print", "block", "macro",
    "format"]

lizSymbols :: Parser Char
lizSymbols = choice
  [char '*', char '/', char '-', char '=', char '+', char '<',
   char '>', char '?', char '.', char '^', char '%', char '$',
   char '#', char '£', char '@', char '!', char '\\', char ',',
   char '~']

parseType :: Parser Type
parseType =
  Int'    <$ string "Int" <|>
  Float'  <$ string "Float" <|>
  String' <$ string "String" <|>
  Char'   <$ string "Char" <|>
  Bool'   <$ string "Bool" <|>
  Unit'   <$ string "Unit"

parseValue :: Parser Expression
parseValue = do
  s <- getCurrentLine
  v <- choice [
      ELiteral String' <$> parseStr, 
      ELiteral Char' <$> parseChar,
      parseNum, 
      ELiteral Bool' <$> parseBool,
      ELiteral Unit' <$> parseUnit,
      EIdentifier <$> parseIdent False
    ]
  e <- getCurrentLine
  pure $ v (LizRange s e)

parseNested :: Parser SExpr
parseNested = do
  v <- (SEExpr <$> (parseValueMacro <|> parseValue)) <|> parseSExpr
  pure v

parseExpr :: Parser Expression
parseExpr = parseValue <|> parseValueMacro <|> (between (char '(') (char ')') $ 
  choice [
    parseRet,
    parsePrint,
    parseFormat,
    parseFuncCall
  ]) 

-- main parsing functions
parseIdent :: Bool -> Parser T.Text
parseIdent shouldParseOp = do
  s <- letterChar
  r <- takeWhileP (Just "alphanumeric character or '_' or '-'.") valid
  let ident = T.pack [s] <> r

  if ident `elem` lizReserved && (not shouldParseOp)
  then reservedIdent ident
  else pure $ ident
  where
    valid :: Char -> Bool 
    valid c = (liftA2 (||) isAlphaNum ('_' ==) c) || ('-' == c)

parseUnit :: Parser T.Text
parseUnit = string "()"

parseStr :: Parser T.Text
parseStr = do
  _ <- char '"'
  str <- takeWhileP (Just "alphanumeric character.") valid
  _ <- char '"'
  pure str
  where
    valid :: Char -> Bool
    valid = liftA2 (&&) isPrint ('"' /=)

parseChar :: Parser T.Text
parseChar = do
  d1 <- char '\''
  c <- printChar
  d2 <- char '\''
  pure $ T.pack [d1, c, d2]

parseNum :: Parser (LizRange -> Expression)
parseNum = do
  n <- (takeWhile1P @_ @T.Text (Just "digits 0-9 or '.'") valid) <* notFollowedBy letterChar
  case () of _
              | count '.' n == 1 -> pure $ ELiteral Float' n
              | count '.' n == 0 -> pure $ ELiteral Int' n
              | otherwise -> invalidNumber n
  where
    valid :: Char -> Bool
    valid = liftA2 (||) isDigit ('.' ==) 

    count :: Char -> T.Text -> Int
    count c = T.length . T.filter (c ==)

parseBool :: Parser T.Text
parseBool = string "True" <|> string "False"

parseValueMacro :: Parser Expression
parseValueMacro = do
  s <- getCurrentLine
  _ <- char '%'
  i <- parseIdent False
  e <- getCurrentLine
  pure $ EValueMacro i (LizRange s e)

parsePrint :: Parser Expression
parsePrint = do
  s <- getCurrentLine
  _ <- string "print"
  hspace1
  v <- parseExpr
  e <- getCurrentLine
  pure $ EPrint (LizRange s e) v

parseRet :: Parser Expression
parseRet = do
  s <- getCurrentLine
  _ <- string "return"
  hspace1
  v <- parseExpr
  e <- getCurrentLine
  pure $ EReturn (LizRange s e) v

parseFormat :: Parser Expression
parseFormat = do
  s <- getCurrentLine
  _ <- string "format"
  hspace1
  fstr <- parseStr
  hspace1
  args <- parseFormatArgs
  e <- getCurrentLine
  pure $ EFormat (LizRange s e) fstr args
  where
    parseFormatArgs :: Parser [Expression]
    parseFormatArgs = parseExpr `sepBy` char ' '

parseMacroDef :: Parser Macro
parseMacroDef = do
  s <- getCurrentLine
  _ <- string "macro"
  hspace1
  i <- parseIdent False
  hspace1
  v <- parseExpr
  e <- getCurrentLine
  pure $ Macro (LizRange s e) i v

parseComment :: Parser SExpr
parseComment = do
  _ <- char ';'
  _ <- hidden $ takeWhileP Nothing valid
  pure SEComment
  where
    valid :: Char -> Bool
    valid = liftA2 (||) isPrint ('\n' /=)

-- TODO: refactor parsing to remove the need for SEType.
parseVarDecl :: Parser (LizRange -> Var -> a) -> Parser Expression -> Parser a
parseVarDecl dec varParse = do
  s <- getCurrentLine
  decType <- dec
  hspace1
  ident <- parseIdent False
  hspace1 
  ty <- (liftM SEType parseType) <|> (liftM SEExpr varParse)
  aux decType s ident ty
  where
    aux decl s iden (SEType ty) = do
      _ <- some hspace1
      value <- L.lineFold scn $ \_ -> varParse
      e <- getCurrentLine
      pure $ decl (LizRange s e) Var{varIdent=iden, varType=ty, varValue=value}
    aux decl s iden (SEExpr lit@(ELiteral ty _ _)) = do
      e <- getCurrentLine
      pure $ decl (LizRange s e) Var{varIdent=iden, varType=ty, varValue=lit}
    aux _ _ _ op = unsupportedDeclaration $ T.show op

parseSetStmt :: Parser SExpr
parseSetStmt = do
  s <- getCurrentLine
  _ <- string "set"
  hspace1
  ident <- parseIdent False
  hspace1
  value <- parseExpr
  e <- getCurrentLine
  pure $ SESet (LizRange s e) ident value

parseFuncDecl :: Parser Func
parseFuncDecl = do
   s <- getCurrentLine
   _ <- string "def"
   hspace1
   ident <- (parseIdent False) <|> parseOpId
   hspace1
   args <- parseFuncArgs
   hidden hspace
   _ <- char '>'
   hidden hspace
   retTy <- parseType

   block <- some $ L.lineFold scn $ \_ -> parseSExpr
   e <- getCurrentLine
   pure $ Func {funcIdent = ident, funcPos = LizRange s e, funcArgs = args, funcReturnType = retTy, funcBody = block}
   where
    parseOpId :: Parser T.Text
    parseOpId = do
      _ <- char '('
      op <- liftM T.pack $ some lizSymbols
      _ <- char ')'
      if op `elem` lizReserved then reservedIdent op
                               else pure op

    parseFuncArgs :: Parser [Arg]
    parseFuncArgs = between (char '[') (char ']') $ aux `sepBy` char ','
      where
       aux :: Parser Arg
       aux = do
         hidden hspace
         ident <- (parseIdent False)
         hidden hspace
         _ <- char '~'
         hidden hspace
         ty <- parseType
         pure Arg {argIdent = ident, argType = ty}

parseFuncCall :: Parser Expression
parseFuncCall = do
  s <- getCurrentLine
  ident <- (parseIdent True) <|> parseOpId
  hspace1
  args <- parseCallArgs
  e <- getCurrentLine
  let
    binaryRes = find ((==) ident . fst) binaryOps
    unaryRes = find ((==) ident . fst) unaryOps
  case binaryRes of
    Just op -> 
        if length args == 2 
        then 
          let 
            l = head' args 
            r = last' args
          in pure $ EBinary (snd op) (LizRange s e) l r
        else wrongArgCount 2 (length args)
    Nothing ->
      case unaryRes of
        Just op ->
          if length args == 1 
          then pure $ EUnary (snd op) (LizRange s e) (head' args) 
          else wrongArgCount 1 (length args)
        Nothing -> pure $ EFuncCall (LizRange s e) ident args
  where
    parseCallArgs :: Parser [Expression]
    parseCallArgs = parseExpr `sepBy` char ' '

    parseOpId :: Parser T.Text
    parseOpId = liftM T.pack $ some lizSymbols

    binaryOps :: [(T.Text, BinaryOp)]
    binaryOps = [
        ("++", Concat),
        ("==", Eql),
        ("!=", NotEql),
        ("+", Add),
        ("-", Subtract),
        ("*", Multiply),
        ("/", Divide),
        (">=", GreaterEql),
        ("<=", LessEql),
        (">", Greater),
        ("<", Less)
      ]

    unaryOps :: [(T.Text, UnaryOp)]
    unaryOps = [
        ("not", Not), 
        ("negate", Negate)
      ]

parseBlock :: Parser SExpr
parseBlock = do
  s <- getCurrentLine
  _ <- string "block"
  hspace
  block <- some $ L.lineFold scn $ \_ -> parseSExpr
  e <- getCurrentLine
  pure $ SEFlow $ FBlockStmt (LizRange s e) (NE.fromList block)

parseIfStmt :: Parser SExpr
parseIfStmt = do
  s <- getCurrentLine
  _ <- string "if"
  hspace
  cond <- parseExpr
  hspace
  block <- some $ L.lineFold scn $ \_ -> parseNested <?> "if-sexpr branch"
  e <- getCurrentLine
  case () of _
              | length block > 2 -> tooManyExprsIf
              | length block == 1 -> pure $ SEFlow $ FIfStmt (LizRange s e) cond (head' block) Nothing
              | otherwise ->
                let 
                  truebr = head' block 
                  falsebr = last' block
                in
                pure $ SEFlow $ FIfStmt (LizRange s e) cond truebr (Just falsebr)

parseSExpr :: Parser SExpr
parseSExpr = (between (char '(') (char ')') $ 
  label "valid S-Expression" 
    (choice [ parseVarDecl (SEVar <$ string "var" <|> SEConst <$ string "const") parseExpr
            , parseSetStmt
            , parseIfStmt
            , SEExpr <$> parseRet
            , SEExpr <$> parsePrint
            , parseBlock
            , SEExpr <$> parseFuncCall
            ])) <|> parseComment

parseProgram :: Parser Program
parseProgram = do
  res <- some $ scn >> aux
  pure $ fold res
  where
    aux :: Parser Program
    aux = 
      (between (char '(') (char ')') $
          (do
            func <- parseFuncDecl
            pure $ Program [func] [] [])
          <|>
          (do
            var <- parseVarDecl (GlblVar <$ string "const") parseValue
            pure $ Program [] [var] [])
          <|>
          (do
            macro <- parseMacroDef
            pure $ Program [] [] [macro]))
      <|> (parseComment >> mempty)

parseFile :: FilePath -> T.Text -> Either (ParseErrorBundle T.Text E.PError) Program
parseFile f fc = do
  case (parse parseProgram f fc) of
    (Left err) -> Left err
    (Right (Program funcs glbls macros)) -> Right $ Program (map aux funcs) glbls macros
  where
    aux :: Func -> Func
    aux fn@Func{funcBody=body} = fn{funcBody = removeComments body}

    removeComments :: [SExpr] -> [SExpr]
    removeComments [] = []
    removeComments (SEComment : rest) = removeComments rest
    removeComments (SEFlow (FBlockStmt r body) : rest) = (SEFlow $ FBlockStmt r (NE.fromList . NE.filter (SEComment /=) $ body)) : removeComments rest
    removeComments (expr : rest) = expr : removeComments rest
