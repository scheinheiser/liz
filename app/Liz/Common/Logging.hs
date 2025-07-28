{-# LANGUAGE OverloadedStrings #-}

module Liz.Common.Logging where

import qualified Text.Colour as C
import qualified Data.Text as T

import Liz.Common.Errors
import Liz.Common.Types (LizPos)

import Text.Megaparsec (unPos)
import Text.Printf (printf)
import Data.List (intercalate, mapAccumL)

sliceFile :: String -> LizPos -> LizPos -> T.Text
sliceFile ftext (sL, sC) (eL, eC) =
  let 
    (sL', sC', eL', eC') = (unPos sL - 1, unPos sC, unPos eL - 1, unPos eC)
    modifyLine n l 
      | n == sL' && n == eL' = drop sC' $ take eC' l
      | n == sL' = drop sC' l
      | n == eL' = take eC' l
      | otherwise = l

    (_, modifiedLines) = mapAccumL (\n l -> (n + 1, modifyLine n l)) 1 $ lines ftext
    slicedFile = grabSlice 0 (\n -> n >= sL' && n <= eL') modifiedLines
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
    IncorrectType s e ex got -> formatError fp (Just (s, e)) (T.pack $ printf "Expected type '%s', but got '%s'." (show ex) (show got)) Nothing (Just $ sliceFile ftext s e)
    IncorrectTypes s e ex got -> formatError fp (Just (s, e)) (T.pack $ printf "Expected types '%s', but got '%s'." ex (intercalate "," $ map show got)) Nothing (Just $ sliceFile ftext s e)
    FailedLitInference s e lit -> formatError fp (Just (s, e)) (T.pack $ printf "Failed to infer the type of '%s'." lit) Nothing (Just $ sliceFile ftext s e)
    UndefinedIdentifier s e i -> formatError fp (Just (s, e)) (T.pack $ printf "Undefined identifier '%s'." i) (Just "Consider checking that the value is in scope. If it is a macro, ensure that it has been defined before its usage.") (Just $ sliceFile ftext s e)
    IdentifierAlreadyInUse s e i -> formatError fp (Just (s, e)) (T.pack $ printf "Identifier '%s' is already in use." i) (Just "Consider giving the identifier a different name.") (Just $ sliceFile ftext s e)
    AssigningToConstant s e i -> formatError fp (Just (s, e)) (T.pack $ printf "Attempted to assign to constant '%s'." i) (Just "Consider defining the value with 'var' instead of 'const'.") (Just $ sliceFile ftext s e)
    AssigningToFunction s e i -> formatError fp (Just (s, e)) (T.pack $ printf "Attempted to assign to the function '%s'." i) (Just $ T.pack $ printf "Did you mean to define '%s' as a function?" i) (Just $ sliceFile ftext s e)
    NotEnoughArgs s e i no -> formatError fp (Just (s, e)) (T.pack $ printf "Not enough args supplied to '%s'." i) (Just $ T.pack $ printf "Missing %s args." (show no)) (Just $ sliceFile ftext s e)
    TooManyArgs s e i no -> formatError fp (Just (s, e)) (T.pack $ printf "Too many args supplied to '%s'." i) (Just $ T.pack $ printf "Supplied an extra %s args." (show no)) (Just $ sliceFile ftext s e)
    IncorrectArgTypes s e i ex got -> 
      let
        formatted_rts = intercalate "," $ map show ex 
        formatted_wts = intercalate "," $ map show got
      in formatError fp (Just (s, e)) (T.pack $ printf "While calling '%s', expectected args of type %s but got %s." i formatted_rts formatted_wts) Nothing (Just $ sliceFile ftext s e)
    NoEntrypoint -> formatError fp Nothing ("Couldn't find entry point.") (Just "Consider adding a 'main' function.") Nothing
    MultipleEntrypoints -> formatError fp Nothing ("Multiple entry points in file.") (Just "Consider renaming one of them.") Nothing
    NotImplemented s -> formatError fp Nothing (T.pack $ printf "SExpression has not been implemented; '%s'." (show s)) Nothing Nothing
    InvalidArgType s e i t -> formatError fp (Just (s, e)) (T.pack $ printf "Invalid type has been given to function argument '%s' - '%s'." i (show t)) (Just "An undefined/unit type arg is disallowed.") (Just $ sliceFile ftext s e)
    RecursiveMacroDef s e i -> formatError fp (Just (s, e)) (T.pack $ printf "'%s' is a recursive macro definition." i) Nothing (Just $ sliceFile ftext s e)
    NonGlblMacroDef s e -> formatError fp (Just (s, e)) "Invalid declaration of macro." (Just "Macros can only be defined locally.") (Just $ sliceFile ftext s e)
  where
    formatError :: FilePath -> Maybe (LizPos, LizPos) -> T.Text -> Maybe T.Text -> Maybe T.Text -> [C.Chunk]
    formatError f (Just (s, e)) errtxt (Just hinttxt) (Just slice) = (filePrefixWithLoc f s e) <> errorPrefix <> (errorText errtxt) <> (hintText hinttxt) <> slicePrefix <> (sliceText slice)
    formatError f (Just (s, e)) errtxt (Just hinttxt) Nothing = (filePrefixWithLoc f s e) <> errorPrefix <> (errorText errtxt) <> (hintText hinttxt)
    formatError f (Just (s, e)) errtxt Nothing (Just slice) = (filePrefixWithLoc f s e) <> errorPrefix <> (errorText errtxt) <> slicePrefix <> (sliceText slice)
    formatError f (Just (s, e)) errtxt Nothing Nothing = (filePrefixWithLoc f s e) <> errorPrefix <> (errorText errtxt)
    formatError f Nothing errtxt (Just hinttxt) (Just slice) = (filePrefixNoLoc f) <> errorPrefix <> (errorText errtxt) <> (hintText hinttxt) <> slicePrefix <> (sliceText slice)
    formatError f Nothing errtxt (Just hinttxt) Nothing = (filePrefixNoLoc f) <> errorPrefix <> (errorText errtxt) <> (hintText hinttxt)
    formatError f Nothing errtxt Nothing (Just slice) = (filePrefixNoLoc f) <> errorPrefix <> (errorText errtxt) <> slicePrefix <> (sliceText slice)
    formatError f Nothing errtxt Nothing Nothing = (filePrefixNoLoc f) <> errorPrefix <> (errorText errtxt)

    filePrefixWithLoc :: FilePath -> LizPos -> LizPos -> [C.Chunk]
    filePrefixWithLoc f (sL, sC) (eL, eC) = [
        C.bold $ C.chunk $ T.pack $ printf "%s@%s:%s-%s:%s\n" f (show $ unPos sL) (show $ unPos sC) (show $ unPos eL) (show $ unPos eC)
      ]

    filePrefixNoLoc :: FilePath -> [C.Chunk]
    filePrefixNoLoc f = [C.bold $ C.chunk $ T.pack $ printf "%s@all\n" f]

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
    slicePrefix = [(C.fore textColour) $ C.chunk $ "  In this code:\n"]

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
