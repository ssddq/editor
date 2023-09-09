{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Format.TypeSignature where

import Parse (ByteString, Expression(..), Function(..), TypeSignature(..), Unit(..), Variable(Knd, Var))

import Data.ByteString.Lazy       qualified as BS hiding (replicate)
import Data.ByteString.Lazy.Char8 qualified as BS

import Data.Int  (Int64)
import Data.List (sortOn)

alignCmts
  :: TypeSignature
  -> TypeSignature
alignCmts (TypeSignature name sig) = TypeSignature name (alignCmtsTo (1 + maxLnLen sig) sig)

alignCmtsTo
  :: Int64
  -> Function
  -> Function
alignCmtsTo n f = case (f) of
  Quantification vars fun      -> Quantification vars (alignCmtsTo n fun)
  Constraint     exp  fun cmts -> Constraint     exp  (alignCmtsTo n fun) (padCmtWithExp n exp cmts)
  Function       exp  fun cmts -> Function       exp  (alignCmtsTo n fun) (padCmtWithExp n exp cmts)
  Expression     exp      cmts -> Expression     exp                      (padCmtWithExp n exp cmts)

alignTprs
  :: TypeSignature
  -> TypeSignature
alignTprs (TypeSignature name sig) = TypeSignature name (alignTprsTo (maxTprLen sig) sig)

alignTprsTo
  :: Int64
  -> Function
  -> Function
alignTprsTo n f = case (f) of
  Quantification vars fun      -> Quantification (vars               ) (alignTprsTo n fun)
  Constraint     exp  fun cmts -> Constraint     (exp                ) (alignTprsTo n fun) cmts
  Function       exp  fun cmts -> Function       (padTprAndSort n exp) (alignTprsTo n fun) cmts
  Expression     exp      cmts -> Expression     (padTprAndSort n exp)                     cmts

formatQtfyVar
  :: Variable
  -> ByteString
formatQtfyVar (Var str    ) = str
formatQtfyVar (Knd str exp) = "(" <> str <> " :: " <> formatTyExp exp <> ")"

formatQtfyVars
  :: [Variable]
  -> ByteString
formatQtfyVars vars = BS.intercalate " " $ map formatQtfyVar vars

formatTprEq
  :: (ByteString, Expression)
  -> ByteString
formatTprEq (lhs, rhs) = lhs <> " = " <> formatTyExp rhs

formatTyDec
  :: TypeSignature
  -> ByteString
formatTyDec dec = formatted
  where
    TypeSignature name sig = alignCmts $ alignTprs dec
    formatted = case (sig) of
      Expression exp     cmts -> name <> " :: "    <> formatTyExp exp <> cmts
      Function   exp fun cmts -> name <> "\n  :: " <> formatTyExp exp <> cmts
                                      <> "\n  -> " <> formatTySig fun
      Constraint exp fun cmts -> name <> "\n  :: " <> formatTyExp exp <> cmts
                                      <> "\n  => " <> formatTySig fun
      Quantification vars fun -> case (fun) of
        Expression exp     cmts -> name <> " :: forall "    <> formatQtfyVars vars <> ". " <> formatTyExp exp <> cmts
        Constraint exp fun cmts -> name <> "\n  :: forall " <> formatQtfyVars vars <> ". " <> formatTyExp exp <> cmts
                                        <> "\n  => "        <> formatTySig fun
        _ -> name <> "\n  :: forall " <> formatQtfyVars vars
                  <> "\n   . "        <> formatTySig fun

formatTyExp
  :: Expression
  -> ByteString
formatTyExp (Unit     grp) = formatTyGrp grp
formatTyExp (Appl exp grp) = formatTyExp exp <> " " <> formatTyGrp grp
formatTyExp (TyEq a   b  ) = formatTyExp a <> " ~ " <> formatTyExp b

formatTyFun
  :: Function
  -> ByteString
formatTyFun f = case f of
  Function   exp fun cmts -> formatTyExp exp <> formatCmts cmts <> " -> " <> formatTyFun fun
  Constraint exp fun cmts -> formatTyExp exp <> formatCmts cmts <> " => " <> formatTyFun fun
  Expression exp     cmts -> formatTyExp exp <> cmts
  Quantification vars fun -> "forall " <> formatQtfyVars vars <> ". " <> formatTyFun fun
  where formatCmts cmts = case (BS.null cmts) of
          True  -> cmts
          False -> cmts <> "\n   "

formatTyGrp
  :: Unit
  -> ByteString
formatTyGrp (Atm str    ) = str
formatTyGrp (Par fn     ) = "(" <> formatTyFun fn <> ")"
formatTyGrp (Lst fn     ) = "[" <> formatTyFun fn <> "]"
formatTyGrp (Tpl fns    ) = "("   <> BS.intercalate ", " (map formatTyFun fns) <>   ")"
formatTyGrp (Utp fns    ) = "(# " <> BS.intercalate ", " (map formatTyFun fns) <> " #)"
formatTyGrp (Tpr str eqs) = str <> " { " <> BS.intercalate ", " (map formatTprEq eqs) <> " }"

formatTySig
  :: Function
  -> ByteString
formatTySig sig = case (sig) of
  Function   exp fun cmts -> formatTyExp exp <> cmts <> "\n  -> " <> formatTySig fun
  Constraint exp fun cmts -> formatTyExp exp <> cmts <> "\n  => " <> formatTySig fun
  _                       -> formatTyFun sig

maxLnLen
  :: Function
  -> Int64
maxLnLen (Function       exp  fun _) = max (tyExpLen exp) $ maxLnLen fun
maxLnLen (Expression     exp      _) = tyExpLen exp
maxLnLen (Constraint     exp  fun _) = max (maxLnLen fun) $ BS.length $ formatTyExp exp
maxLnLen (Quantification vars fun  ) = case (fun) of
  Expression exp        _ -> (BS.length $ "forall " <> formatQtfyVars vars <> ". ") + tyExpLen exp
  Constraint constr fun _ -> max (maxLnLen fun) $ BS.length $ "forall " <> formatQtfyVars vars <> ". " <> formatTyExp constr
  _  -> max (maxLnLen fun) $ BS.length $ "forall " <> formatQtfyVars vars

maxTprLen
  :: Function
  -> Int64
maxTprLen (Function       exp fun _) = max (tlrLen exp) $ maxTprLen fun
maxTprLen (Expression     exp     _) = tlrLen exp
maxTprLen (Constraint     _   fun _) = maxTprLen fun
maxTprLen (Quantification _   fun  ) = maxTprLen fun

padCmtWithExp
  :: Int64
  -> Expression
  -> ByteString
  -> ByteString
padCmtWithExp n exp cmt = padding <> cmt
  where padding = BS.replicate (n - (BS.length $ formatTyExp exp)) ' '

padTprAndSort
  :: Int64
  -> Expression
  -> Expression
padTprAndSort n e = case (e) of
  Unit     (Tpr str eqns) -> Unit     $ Tpr (str <> padStr str    ) (sortOn fst eqns)
  Appl exp (Tpr str eqns) -> Appl exp $ Tpr (str <> padExp str exp) (sortOn fst eqns)
  exp                     -> exp
  where padStr str     = BS.replicate (n - BS.length str                            ) ' '
        padExp str exp = BS.replicate (n - BS.length (formatTyExp exp <> " " <> str)) ' '

padTprTo
  :: Int64
  -> Expression
  -> Expression
padTprTo n e = case (e) of
  Unit     (Tpr str eqns) -> Unit     $ Tpr (str <> padStr str    ) eqns
  Appl exp (Tpr str eqns) -> Appl exp $ Tpr (str <> padExp str exp) eqns
  exp                     -> exp
  where padStr str     = BS.replicate (n - BS.length str                            ) ' '
        padExp str exp = BS.replicate (n - BS.length (formatTyExp exp <> " " <> str)) ' '

tlrLen
  :: Expression
  -> Int64
tlrLen (Unit     (Tpr str _)) = BS.length $ str
tlrLen (Appl exp (Tpr str _)) = BS.length $ formatTyExp exp <> " " <> str
tlrLen TyEq{}                 = 0
tlrLen _                      = 0

tyExpLen
  :: Expression
  -> Int64
tyExpLen exp = BS.length $ formatTyExp exp
