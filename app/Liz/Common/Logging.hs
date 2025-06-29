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
    IncorrectType s e ex got -> formatError fp (Just (s, e)) (T.pack $ printf "Expected type '%s', but got '%s'.\n" (show ex) (show got)) Nothing
    IncorrectTypes s e ex got -> formatError fp (Just (s, e)) (T.pack $ printf "Expected types '%s', but got '%s'.\n" ex (intercalate "," $ map show got)) Nothing
    FailedLitInference s e lit -> formatError fp (Just (s, e)) (T.pack $ printf "Failed to infer the type of '%s'.\n" lit) Nothing
    UndefinedIdentifier s e i -> formatError fp (Just (s, e)) (T.pack $ printf "Undefined identifier '%s'." i) (Just "Consider checking that the value is in scope.\n")
    IdentifierAlreadyInUse s e i -> formatError fp (Just (s, e)) (T.pack $ printf "Identifier '%s' is already in use." i) (Just "Consider giving the identifier a different name.")
    AssigningToConstant s e i -> formatError fp (Just (s, e)) (T.pack $ printf "Attempted to assign to constant '%s'." i) (Just "Consider defining the value with 'var' instead of 'const'.\n")
    AssigningToFunction s e i -> formatError fp (Just (s, e)) (T.pack $ printf "Attempted to assign to the function '%s'." i) (Just $ T.pack $ printf "Did you mean to define '%s' as a function?\n" i)
    NotEnoughArgs s e i no -> formatError fp (Just (s, e)) (T.pack $ printf "Not enough args supplied to '%s'." i) (Just $ T.pack $ printf "Missing %s args.\n" (show no))
    TooManyArgs s e i no -> formatError fp (Just (s, e)) (T.pack $ printf "Too many args supplied to '%s'." i) (Just $ T.pack $ printf "Supplied an extra %s args.\n" (show no))
    IncorrectArgTypes s e i ex got -> 
      let
        formatted_rts = intercalate "," $ map show ex 
        formatted_wts = intercalate "," $ map show got
      in formatError fp (Just (s, e)) (T.pack $ printf "While calling '%s', expectected args of type %s but got %s.\n" i formatted_rts formatted_wts) Nothing
    NoEntrypoint -> formatError fp Nothing ("Couldn't find entry point.") (Just "Consider adding a 'main' function.\n")
    MultipleEntrypoints -> formatError fp Nothing ("Multiple entry points in file.") (Just "Consider renaming one of them.\n")
    NotImplemented s -> formatError fp Nothing (T.pack $ printf "SExpression has not been implemented; '%s'.\n" (show s)) Nothing
  where
    formatError :: FilePath -> Maybe (LizPos, LizPos) -> T.Text -> Maybe T.Text -> [C.Chunk]
    formatError f (Just (s, e)) errtxt (Just hinttxt) = (filePrefixWithLoc f s e) <> errorPrefix <> (errorText errtxt) <> (hintText hinttxt)
    formatError f (Just (s, e)) errtxt Nothing = (filePrefixWithLoc f s e) <> errorPrefix <> (errorText errtxt)
    formatError f Nothing errtxt (Just hinttxt) = (filePrefixNoLoc f) <> errorPrefix <> (errorText errtxt) <> (hintText hinttxt)
    formatError f Nothing errtxt Nothing = (filePrefixNoLoc f) <> errorPrefix <> (errorText errtxt)

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
