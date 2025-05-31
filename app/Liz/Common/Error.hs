{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE LambdaCase #-}

module Liz.Common.Error where

import qualified Data.Text as T
import qualified Text.Colour as C

import Liz.Common.Types
import Text.Printf (printf)
import Data.List (intercalate)
import Text.Megaparsec

data SemErr = IncorrectType LizPos LizPos Type Type -- expected type ; given type
  | IncorrectTypes LizPos LizPos T.Text [Type] -- expected types ; given type
  | FailedLitInference LizPos LizPos T.Text
  | UndefinedIdentifier LizPos LizPos T.Text
  | IdentifierAlreadyInUse LizPos LizPos T.Text
  | AssigningToConstant LizPos LizPos T.Text
  | AssigningToFunction LizPos LizPos T.Text
  | NotEnoughArgs LizPos LizPos T.Text Int
  | TooManyArgs LizPos LizPos T.Text Int
  | IncorrectArgTypes LizPos LizPos T.Text [Type] [Type] -- expected types ; given types
  | NoEntrypoint
  | NotImplemented SExpr
  deriving (Show, Eq)

prettifyErr :: SemErr -> FilePath -> [C.Chunk]
prettifyErr expr fp =
  case expr of
    IncorrectType s e ex got -> (filePrefixWithLoc fp s e) <> errorPrefix <> (errorText $ T.pack $ printf "Expected type '%s', but got '%s'.\n" (show ex) (show got))
    IncorrectTypes s e ex got -> (filePrefixWithLoc fp s e) <> errorPrefix <> (errorText $ T.pack $ printf "Expected types '%s', but got '%s'.\n" ex (intercalate "," $ map show got))
    FailedLitInference s e lit -> (filePrefixWithLoc fp s e) <> errorPrefix <> (errorText $ T.pack $ printf "Failed to infer the type of '%s'.\n" lit)
    UndefinedIdentifier s e i -> (filePrefixWithLoc fp s e) <> errorPrefix <> (errorText $ T.pack $ printf "Undefined identifier '%s'." i) <> (hintText "Consider checking that the value is in scope.\n")
    IdentifierAlreadyInUse s e i -> (filePrefixWithLoc fp s e) <> errorPrefix <> (errorText $ T.pack $ printf "Identifier '%s' is already in use." i) <> (hintText "Consider giving the identifier a different name.")
    AssigningToConstant s e i -> (filePrefixWithLoc fp s e) <> errorPrefix <> (errorText $ T.pack $ printf "Attempted to assign to constant '%s'." i) <> (hintText "Consider defining the value with 'var' instead of 'const'.\n")
    AssigningToFunction s e i -> (filePrefixWithLoc fp s e) <> errorPrefix <> (errorText $ T.pack $ printf "Attempted to assign to the function '%s'." i) <> (hintText $ T.pack $ printf "Did you mean to define '%s' as a function?\n" i)
    NotEnoughArgs s e i no -> (filePrefixWithLoc fp s e) <> errorPrefix <> (errorText $ T.pack $ printf "Not enough args supplied to '%s'." i) <> (hintText $ T.pack $ printf "Missing %s args.\n" (show no))
    TooManyArgs s e i no -> (filePrefixWithLoc fp s e) <> errorPrefix <> (errorText $ T.pack $ printf "Too many args supplied to '%s'." i) <> (hintText $ T.pack $ printf "Supplied an extra %s args.\n" (show no))
    IncorrectArgTypes s e i ex got -> 
      let
        formatted_rts = intercalate "," $ map show ex 
        formatted_wts = intercalate "," $ map show got
      in (filePrefixWithLoc fp s e) <> errorPrefix <> (errorText $ T.pack $ printf "While calling '%s', expectected args of type %s but got %s.\n" i formatted_rts formatted_wts)
    NoEntrypoint -> (filePrefixNoLoc fp) <> errorPrefix <> (errorText "Couldn't find entry point.") <> (hintText "Consider adding a 'main' function.\n")
    NotImplemented s -> (filePrefixNoLoc fp) <> errorPrefix <> (errorText $ T.pack $ printf "SExpression has not been implemented; '%s'.\n" (show s))
  where
    filePrefixWithLoc :: FilePath -> LizPos -> LizPos -> [C.Chunk]
    filePrefixWithLoc f (sL, sC) (eL, eC) = [
        (C.underline . C.bold . C.fore fpColour) $ C.chunk $ T.pack $ printf "%s@%s:%s-%s:%s\n" f (show $ unPos sL) (show $ unPos sC) (show $ unPos eL) (show $ unPos eC)
      ]
      where
        fpColour :: C.Colour
        fpColour = C.colourRGB 70 214 17

    filePrefixNoLoc :: FilePath -> [C.Chunk]
    filePrefixNoLoc f = [(C.underline . C.bold . C.fore fpColour) $ C.chunk $ T.pack $ printf "%s@all\n" f]
      where
        fpColour :: C.Colour
        fpColour = C.colourRGB 70 214 17

    errorPrefix :: [C.Chunk]
    errorPrefix = [
        (C.fore errorColour) $ C.chunk "[ERROR]",
        (C.fore textColour) $ C.chunk " ~ "
      ]

    errorText :: T.Text -> [C.Chunk]
    errorText t = [(C.underline . C.bold . C.fore textColour) $ C.chunk (t <> "\n")]

    hintText :: T.Text -> [C.Chunk]
    hintText t = [C.fore hintColour $ C.chunk "  [HINT] ~ ", C.fore hintColour $ C.chunk (t <> "\n")]

    errorColour :: C.Colour
    errorColour = C.colourRGB 237 26 33 

    textColour :: C.Colour
    textColour = C.colourRGB 237 228 228

    hintColour :: C.Colour
    hintColour = C.colourRGB 217 190 150

printErrs :: FilePath -> [SemErr] -> [C.Chunk] -> IO ()
printErrs _ [] acc = C.putChunksUtf8With C.With24BitColours acc
printErrs fp (e : es) acc = printErrs fp es (prettifyErr e fp <> acc)

data PError = FailedTypeInference T.Text
  | ReservedIdent T.Text
  | UnsupportedDeclaration T.Text
  | InferredUndefined
  deriving (Show, Eq, Ord)

instance ShowErrorComponent PError where
  showErrorComponent = \case
    ReservedIdent s ->          printf "[ERROR] Expected identifier, found keyword '%s'" (T.unpack s)
    FailedTypeInference s ->    printf "[ERROR] Failed to infer type of '%s'" (T.unpack s)
    UnsupportedDeclaration s -> printf "[ERROR] This type of declaration is currently not supported - '%s'" (T.unpack s)
    InferredUndefined ->        printf "[ERROR] Can't infer the type of undefined."
