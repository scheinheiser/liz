{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE LambdaCase #-}

module Liz.Common.Error (PError (..), SemErr (..), printErrs) where

import qualified Data.Text as T
import Data.List
import qualified Error.Diagnose as D
import Error.Diagnose.Diagnostic

import Liz.Common.Types
import Text.Printf (printf)
import Text.Megaparsec

data SemErr = MismatchedTypes LizPos LizPos Type T.Text -- expected type ; given type
  | IncorrectType LizPos LizPos Type Type -- expected type ; given type
  | IncorrectTypes LizPos LizPos T.Text [Type] -- expected types ; given type
  | FailedLitInference LizPos LizPos T.Text
  | UndefinedIdentifier LizPos LizPos T.Text
  | UndefinedFunction LizPos LizPos T.Text
  | IdentifierAlreadyInUse LizPos LizPos T.Text
  | AssigningToConstant LizPos LizPos T.Text
  | AssigningToFunction LizPos LizPos T.Text
  | NotEnoughArgs LizPos LizPos T.Text Int
  | TooManyArgs LizPos LizPos T.Text Int
  | IncorrectArgTypes LizPos LizPos T.Text [Type] [Type] -- expected types ; given types
  | NoEntrypoint
  | NotImplemented SExpr
  deriving (Show, Eq)

-- TODO: switch to prettyprinter, diagnose isn't as flexible as it.
prettifyErr :: SemErr -> FilePath -> D.Report T.Text
prettifyErr (MismatchedTypes (sL, sC) (eL, eC) ex got) f = 
  D.err Nothing (T.pack $ printf "Expected a value of type '%s', but got '%s'" (show ex) got) 
    [(D.Position (unPos sL, unPos sC) (unPos eL, unPos eC) f, D.This "While checking this expression.")] 
    mempty
prettifyErr (IncorrectType (sL, sC) (eL, eC) ex got) f =
  D.err Nothing (T.pack $ printf "Expected a value of type '%s', but got '%s'" (show ex) (show got)) 
    [(D.Position (unPos sL, unPos sC) (unPos eL, unPos eC) f, D.This "While checking this expression.")] 
    mempty
prettifyErr (IncorrectTypes (sL, sC) (eL, eC) ex got) f =
  let formatted_types = intercalate "," $ map show got in
  D.err Nothing (T.pack $ printf "Expected a value of types '%s', but got '%s'" (show ex) formatted_types) 
    [(D.Position (unPos sL, unPos sC) (unPos eL, unPos eC) f, D.This "While checking this expression.")] 
    mempty
prettifyErr (FailedLitInference (sL, sC) (eL, eC) lit) f =
  D.err Nothing (T.pack $ printf "Failed to infer the type of '%s'" lit) 
    [(D.Position (unPos sL, unPos sC) (unPos eL, unPos eC) f, D.This "While checking this literal.")] 
    mempty
prettifyErr (UndefinedIdentifier (sL, sC) (eL, eC) iden) f =
  D.err Nothing (T.pack $ printf "'%s' was not declared within the scope." iden) 
    [(D.Position (unPos sL, unPos sC) (unPos eL, unPos eC) f, D.This "While checking this identifier.")] 
    [T.pack $ printf "Consider defining '%s' globally or within the function." iden]
prettifyErr (UndefinedFunction (sL, sC) (eL, eC) iden) f =
  D.err Nothing (T.pack $ printf "'%s' has not been defined in the file." iden) 
    [(D.Position (unPos sL, unPos sC) (unPos eL, unPos eC) f, D.This "While checking this identifier.")] 
    [T.pack $ printf "Consider defining '%s' within the file." iden]
prettifyErr (IdentifierAlreadyInUse (sL, sC) (eL, eC) iden) f =
  D.err Nothing (T.pack $ printf "The identifier '%s' is already in use." iden) 
    [(D.Position (unPos sL, unPos sC) (unPos eL, unPos eC) f, D.This "While checking this identifier.")] 
    ["Consider renaming the variable."]
prettifyErr (AssigningToConstant (sL, sC) (eL, eC) iden) f =
  D.err Nothing (T.pack $ printf "Cannot assign to constant '%s'." iden) 
    [(D.Position (unPos sL, unPos sC) (unPos eL, unPos eC) f, D.This "While checking this identifier.")] 
    ["Consider defining the variable with 'var' instead of 'const'."]
prettifyErr (AssigningToFunction (sL, sC) (eL, eC) iden) f =
  D.err Nothing (T.pack $ printf "Cannot assign to the function '%s'." iden) 
    [(D.Position (unPos sL, unPos sC) (unPos eL, unPos eC) f, D.This "While checking this identifier.")] 
    [T.pack $ printf "Did you mean to define '%s' as a function?" iden]
prettifyErr (NotEnoughArgs (sL, sC) (eL, eC) iden no) f =
  D.err Nothing (T.pack $ printf "Not enough args supplied to %s." iden) 
    [(D.Position (unPos sL, unPos sC) (unPos eL, unPos eC) f, D.This "While checking this function call.")] 
    [T.pack $ printf "Missing %s args." no]
prettifyErr (TooManyArgs (sL, sC) (eL, eC) iden no) f =
  D.err Nothing (T.pack $ printf "Too many args supplied to '%s'." iden) 
    [(D.Position (unPos sL, unPos sC) (unPos eL, unPos eC) f, D.This "While checking this identifier")] 
    [T.pack $ printf "Supplied an extra %s args." no]
prettifyErr (IncorrectArgTypes (sL, sC) (eL, eC) iden r w) f =
  let 
    formatted_rts = intercalate "," $ map show r
    formatted_wts = intercalate "," $ map show w
  in
  D.err Nothing (T.pack $ printf "While calling '%s', expected args of type %s but got %s" iden formatted_rts formatted_wts) 
    [(D.Position (unPos sL, unPos sC) (unPos eL, unPos eC) f, D.This "While checking this identifier")] 
    mempty
-- prettifyErr NoEntrypoint f =
--   D.err Nothing (T.pack $ printf "No entry point found for the program.") 
--     [(D.Position (1, 1) (1, 1) f, D.This "")] 
--    ["Consider writing a 'main' function."] 

printErrs :: [SemErr] -> Diagnostic T.Text -> FilePath -> IO ()
printErrs [] r _ = D.printDiagnostic D.stderr True True 4 D.defaultStyle r
printErrs (x : xs) r f = printErrs xs (addErr x r) f
  where
    addErr :: SemErr -> Diagnostic T.Text -> Diagnostic T.Text
    addErr err d = D.addReport d (prettifyErr err f)

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
