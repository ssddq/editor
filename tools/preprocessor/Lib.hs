{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Lib where


import Control.Monad

import Data.List

import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax

deriving instance Lift (TyVarBndr ())
deriving instance Lift (TyVarBndr Specificity)
deriving instance Lift (BndrVis)
deriving instance Lift (TyVarBndr BndrVis)
deriving instance Lift Name
deriving instance Lift TyLit
deriving instance Lift OccName
deriving instance Lift NameFlavour
deriving instance Lift ModName
deriving instance Lift NameSpace
deriving instance Lift PkgName
deriving instance Lift Specificity
deriving instance Lift Type


appendToFunctionName
  :: Name
  -> Dec
  -> Dec
appendToFunctionName name (ValD (VarP fname) body decs) = ValD (VarP . mkName $ nameBase fname ++ nameBase name) body decs
appendToFunctionName name (SigD fname ty)               = SigD (mkName $ nameBase fname ++ nameBase name) ty
appendToFunctionName ____ dec                           = dec

-- | Generates auxiliary functions for a collection of data declarations in a TH splice.

generate
  :: (Quote m)
  => m [Dec]
  -> m [Dec]
generate decsQ = do
  decQ <- decsQ
  generateReplaceAll decQ

-- | Generates auxiliary functions for a data declaration.

generateReplace
  :: (Quote m)
  => Dec
  -> m [Dec]
generateReplace (DataD _ tyName tyVars _ _ _) = liftM (fmap (appendToFunctionName tyName)) [d|
  replace_ :: (Quote m) => [(String,String)] -> m Type
  replace_ = replaceAll (tyDec tyName tyVars) |]
generateReplace _dec = error "generateReplace called on a declaration that was not a data declaration"

-- | Generates auxiliary functions for a list of data declarations.

generateReplaceAll
  :: (Quote m)
  => [Dec]
  -> m [Dec]
generateReplaceAll [] = return []
generateReplaceAll (dec@(DataD _ _ _ _ _ _) : decs) = do
  replacerDec <- generateReplace dec
  rest        <- generateReplaceAll decs
  return $ dec : replacerDec ++ rest
generateReplaceAll (dec : decs) = do
  rest <- generateReplaceAll decs
  return $ dec : rest

-- | Quantifies over all unbound type variables in a signature.

quantify
  :: (Quote m)
  => m Type
  -> m Type
quantify = fmap quantifyType

-- | Quantifies over all unbound type variables in a signature,
-- | but then removes those in the given list.
-- |
-- | This is meant to be used to not quantify over variables
-- | that have already been quantified over when reconstructing a type signature,
-- | since e.g. simply calling quantify on the body of (forall a. Foo a b)
-- | would quantify over both a and b.
-- |
-- | This seems easier than simply discarding the original quantifiers,
-- | since those might include kind signatures.

quantifyExcept
  :: (Quote m)
  => [String]
  -> m Type
  -> m Type
quantifyExcept names ty = do
  fullyQuantified <- quantify ty
  case fullyQuantified of
    ForallT tyVarBndrs cxt tyQ -> return $ ForallT (removeTyVarBndrs names tyVarBndrs) cxt tyQ
    other                      -> return $ other

-- | Removes a given list of quantified variables from the quantification binding.

removeTyVarBndrs
  :: [String]
  -> [TyVarBndr flag]
  -> [TyVarBndr flag]
removeTyVarBndrs [] bndrs = bndrs
removeTyVarBndrs _ [] = []
removeTyVarBndrs names (PlainTV bndr flag :bndrs)
  | elem (nameBase bndr) names = removeTyVarBndrs names bndrs
  | otherwise                  = (PlainTV bndr flag) : removeTyVarBndrs names bndrs
removeTyVarBndrs names (KindedTV bndr flag kind :bndrs)
  | elem (nameBase bndr) names = removeTyVarBndrs names bndrs
  | otherwise                  = (KindedTV bndr flag kind) : removeTyVarBndrs names bndrs

replace
  :: Type
  -> (String, String)
  -> Type
replace (AppT origType (VarT var)) (original, replacement)
  | nameBase var == original = AppT (replace origType (original, replacement))
                                    (replacementParsed                       )
  | otherwise                = AppT (replace origType (original, replacement))
                                    (VarT $ mkName $ nameBase var            )
  where replacementParsed = case (parseType replacement) of
                              Left  _ -> error "parseType failed"
                              Right r -> r
replace (AppT origType appType) (original, replacement) = AppT (replace origType (original,replacement))
                                                               (appType                                )
replace origType _ = origType

replaceAll
  :: (Quote m)
  => Type
  -> [(String, String)]
  -> m Type
replaceAll origType strings = return $ foldl' (replace )
                                              (origType) strings

scopedTypeVariable
  :: String
  -> Q Type
scopedTypeVariable s = do
  tyName <- lookupTypeName s
  case tyName of
    Just name -> return $ VarT name
    Nothing   -> return $ VarT (mkName s)

tyDec
  :: Name
  -> [TyVarBndr flag]
  -> Type
tyDec tyName tyVars = foldl' AppT (ConT . mkName . nameBase $ tyName )
                                  (fmap (VarT . tyVarBndrName) tyVars)

tyVarBndrName
  :: TyVarBndr flag
  -> Name
tyVarBndrName (PlainTV  name _  ) = name
tyVarBndrName (KindedTV name _ _) = name
