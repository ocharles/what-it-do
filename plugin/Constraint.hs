{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Constraint where

import GhcPlugins
import TcRnTypes
import HsExpr
import HsBinds
import HsExtension
import PrelNames
import ConLike

import TcRnMonad
import TcEnv
import Unique
import Control.Arrow
import TcEvidence
import TcSMonad
import TcSimplify
import DsBinds
import Bag

import Generics.SYB hiding (empty)


generateDictionary :: TcM TcEvBinds
generateDictionary = do
  showTyCon <- tcLookupTyCon showClassName
  dictName <- newName (mkDictOcc (mkVarOcc "magic"))
  let dict_ty = (mkTyConApp showTyCon [ unitTy ])
      dict_var = mkVanillaGlobal dictName dict_ty
  getDictionaryBindings dict_var dict_ty


-- Pass in "Show ()" for example
getDictionaryBindings :: Var -> Type -> TcM TcEvBinds
getDictionaryBindings dict_var dictTy = do

    loc <- getCtLocM (GivenOrigin UnkSkol) Nothing
    let nonC = mkNonCanonical CtWanted
            { ctev_pred = varType dict_var
            , ctev_nosh = WDeriv
            , ctev_dest = EvVarDest dict_var
            , ctev_loc = loc
            }
        wCs = mkSimpleWC [cc_ev nonC]
    (_, evBinds) <- second evBindMapBinds <$> runTcS (solveWanteds wCs)
    return (EvBinds evBinds)


install :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
install _ _ tc_gbl = do
  let binds = tcg_binds tc_gbl
  let res = checkBinds binds
  generateDictionary
  return tc_gbl

checkBinds :: LHsBinds GhcTc -> [LHsExpr GhcTc]
checkBinds lhs_binds =
  listify checkCoerce lhs_binds

castExpr :: Typeable r => r -> Maybe (LHsExpr GhcTc)
castExpr = cast

checkCoerce :: Typeable r => r -> Bool
checkCoerce r =
  case castExpr r of
    Just b -> checkExpr b
    Nothing -> False

ignoreWrapper :: LHsExpr GhcTc -> HsExpr GhcTc
ignoreWrapper (L _ (HsWrap _ _ e)) = e
ignoreWrapper w = unLoc w

pattern CL :: DataCon -> LHsExpr GhcTc
pattern CL dc <- (ignoreWrapper -> HsConLikeOut _ (RealDataCon dc))

pattern MapApp :: IdP GhcTc -> DataCon -> LHsExpr GhcTc
pattern MapApp var r <- (ignoreWrapper -> (HsApp _ ((ignoreWrapper -> (HsVar _ (L _ var))))
                                                   (((CL r)))))

checkExpr :: LHsExpr GhcTc -> Bool
checkExpr (MapApp var dc)
  | (getName var) == mapName
  , isNewTyCon (dataConTyCon dc)
  = True
checkExpr _ = False
