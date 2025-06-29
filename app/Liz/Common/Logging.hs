{-# LANGUAGE OverloadedStrings #-}

module Liz.Common.Logging where

import qualified Text.Colour as C
import qualified Data.Text as T

import Liz.Common.Errors
import Liz.Common.Types (LizPos)

import Text.Megaparsec (unPos)
import Text.Printf (printf)
import Data.List (intercalate)

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
