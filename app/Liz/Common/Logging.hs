{-# LANGUAGE OverloadedStrings #-}

module Liz.Common.Logging where

import qualified Text.Colour as C
import qualified Data.Text as T

import Liz.Common.Errors
import Liz.Common.Types (LizRange (..))

import Text.Printf (printf)
import Data.List (intercalate)

sliceFile :: String -> LizRange -> T.Text
sliceFile ftext (LizRange sL eL) =
  let 
    (sL', eL') = (sL - 1, eL - 1)
    slicedFile = grabSlice 0 (\n -> n >= sL' && n <= eL') $ lines ftext
    slicedFile' =
      if length slicedFile > 3 then (take 3 slicedFile) <> ["..."]
                               else slicedFile
  in T.unlines . map (T.pack . (<>) "  ") $ slicedFile'
  where
    grabSlice :: Int -> (Int -> Bool) -> [a] -> [a]
    grabSlice _ _ [] = []
    grabSlice n p (x : xs) | p n = x : grabSlice (n + 1) p xs
                           | otherwise = grabSlice (n + 1) p xs

prettifyErr :: SemErr -> FilePath -> String -> [C.Chunk]
prettifyErr expr fp ftext =
  case expr of
    IncorrectType range ex got -> formatError fp (Just range) (T.pack $ printf "Expected type '%s', but got '%s'." (show ex) (show got)) Nothing (Just $ sliceFile ftext range)
    IncorrectTypes range ex got -> formatError fp (Just range) (T.pack $ printf "Expected types '%s', but got '%s'." ex (intercalate "," $ map show got)) Nothing (Just $ sliceFile ftext range)
    FailedLitInference range lit -> formatError fp (Just range) (T.pack $ printf "Failed to infer the type of '%s'." lit) Nothing (Just $ sliceFile ftext range)
    UndefinedIdentifier range i -> formatError fp (Just range) (T.pack $ printf "Undefined identifier '%s'." i) (Just "Consider checking that the value is in scope. If it is a macro, ensure that it has been defined before its usage.") (Just $ sliceFile ftext range)
    IdentifierAlreadyInUse range i -> formatError fp (Just range) (T.pack $ printf "Identifier '%s' is already in use." i) (Just "Consider giving the identifier a different name.") (Just $ sliceFile ftext range)
    AssigningToConstant range i -> formatError fp (Just range) (T.pack $ printf "Attempted to assign to constant '%s'." i) (Just "Consider defining the value with 'var' instead of 'const'.") (Just $ sliceFile ftext range)
    AssigningToFunction range i -> formatError fp (Just range) (T.pack $ printf "Attempted to assign to the function '%s'." i) (Just $ T.pack $ printf "Did you mean to define '%s' as a function?" i) (Just $ sliceFile ftext range)
    NotEnoughArgs range i no -> formatError fp (Just range) (T.pack $ printf "Not enough args supplied to '%s'." i) (Just $ T.pack $ printf "Missing %s args." (show no)) (Just $ sliceFile ftext range)
    TooManyArgs range i no -> formatError fp (Just range) (T.pack $ printf "Too many args supplied to '%s'." i) (Just $ T.pack $ printf "Supplied an extra %s args." (show no)) (Just $ sliceFile ftext range)
    IncorrectArgTypes range i ex got -> 
      let
        formatted_rts = intercalate "," $ map show ex 
        formatted_wts = intercalate "," $ map show got
      in formatError fp (Just range) (T.pack $ printf "While calling '%s', expectected args of type %s but got %s." i formatted_rts formatted_wts) Nothing (Just $ sliceFile ftext range)
    NoEntrypoint -> formatError fp Nothing ("Couldn't find entry point.") (Just "Consider adding a 'main' function.") Nothing
    MultipleEntrypoints -> formatError fp Nothing ("Multiple entry points in file.") (Just "Consider renaming one of them.") Nothing
    NotImplemented s -> formatError fp Nothing (T.pack $ printf "SExpression has not been implemented; '%s'." (show s)) Nothing Nothing
    InvalidArgType range i t -> formatError fp (Just range) (T.pack $ printf "Invalid type has been given to function argument '%s' - '%s'." i (show t)) (Just "An undefined/unit type arg is disallowed.") (Just $ sliceFile ftext range)
    RecursiveMacroDef range i -> formatError fp (Just range) (T.pack $ printf "'%s' is a recursive macro definition." i) Nothing (Just $ sliceFile ftext range)
    NonGlblMacroDef range -> formatError fp (Just range) "Invalid declaration of macro." (Just "Macros can only be defined locally.") (Just $ sliceFile ftext range)
    InvalidExpression range -> undefined
  where
    formatError :: FilePath -> Maybe LizRange -> T.Text -> Maybe T.Text -> Maybe T.Text -> [C.Chunk]
    formatError f (Just range) errtxt (Just hinttxt) (Just slice) = (filePrefixWithLoc f range) <> errorPrefix <> (errorText errtxt) <> (hintText hinttxt) <> slicePrefix <> (sliceText slice)
    formatError f (Just range) errtxt (Just hinttxt) Nothing = (filePrefixWithLoc f range) <> errorPrefix <> (errorText errtxt) <> (hintText hinttxt)
    formatError f (Just range) errtxt Nothing (Just slice) = (filePrefixWithLoc f range) <> errorPrefix <> (errorText errtxt) <> slicePrefix <> (sliceText slice)
    formatError f (Just range) errtxt Nothing Nothing = (filePrefixWithLoc f range) <> errorPrefix <> (errorText errtxt)
    formatError f Nothing errtxt (Just hinttxt) (Just slice) = (filePrefixNoLoc f) <> errorPrefix <> (errorText errtxt) <> (hintText hinttxt) <> slicePrefix <> (sliceText slice)
    formatError f Nothing errtxt (Just hinttxt) Nothing = (filePrefixNoLoc f) <> errorPrefix <> (errorText errtxt) <> (hintText hinttxt)
    formatError f Nothing errtxt Nothing (Just slice) = (filePrefixNoLoc f) <> errorPrefix <> (errorText errtxt) <> slicePrefix <> (sliceText slice)
    formatError f Nothing errtxt Nothing Nothing = (filePrefixNoLoc f) <> errorPrefix <> (errorText errtxt)

    filePrefixWithLoc :: FilePath -> LizRange -> [C.Chunk]
    filePrefixWithLoc f (LizRange sL eL) = 
      let 
        message = 
          if sL == eL then printf "%s;\nLine %s:\n" f (show sL)
                      else printf "%s;\nLine %s to line %s:\n" f (show sL) (show eL)
      in [C.bold $ C.chunk $ T.pack message]

    filePrefixNoLoc :: FilePath -> [C.Chunk]
    filePrefixNoLoc f = [C.bold $ C.chunk $ T.pack $ printf "%s:\n" f]

    errorPrefix :: [C.Chunk]
    errorPrefix = [
        (C.fore errorColour) $ C.chunk "  [ERROR]",
        (C.fore textColour) $ C.chunk " ~ "
      ]

    errorText :: T.Text -> [C.Chunk]
    errorText t = [(C.underline . C.bold . C.fore textColour) $ C.chunk (t <> "\n")]

    hintText :: T.Text -> [C.Chunk]
    hintText t = [C.fore hintColour $ C.chunk "  [HINT] ~ ", C.fore hintColour $ C.chunk (t <> "\n")]

    slicePrefix :: [C.Chunk]
    slicePrefix = [(C.fore textColour) $ C.chunk $ "  In this snippet:\n"]

    sliceText :: T.Text -> [C.Chunk]
    sliceText txt = [(C.fore C.red) $ C.chunk $ txt <> "\n"]

    errorColour :: C.Colour
    errorColour = C.colourRGB 237 26 33 

    textColour :: C.Colour
    textColour = C.colourRGB 237 228 228

    hintColour :: C.Colour
    hintColour = C.colourRGB 217 190 150

printErrs :: FilePath -> String -> [SemErr] -> [C.Chunk] -> IO ()
printErrs _ _ [] acc = C.putChunksUtf8With C.With24BitColours acc
printErrs fp ftext (e : es) acc = printErrs fp ftext es (prettifyErr e fp ftext <> acc)
