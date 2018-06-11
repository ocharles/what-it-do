{-# LANGUAGE TemplateHaskell #-}

module WhatItDo ( plugin, traceDo ) where

-- assert-explainer
import qualified Constraint

-- base
import Data.Traversable ( for )
import Control.Monad.IO.Class ( liftIO )
import Data.Foldable ( toList )
import Debug.Trace ( trace )

-- ghc
import qualified Outputable as PP
import qualified Convert as GHC
import qualified CoreUtils
import qualified Desugar as GHC
import qualified Finder as GHC
import qualified GHC
import qualified GhcPlugins as GHC
import qualified HsExpr as Expr
import qualified IfaceEnv as GHC
import qualified PrelNames as GHC
import qualified RnExpr as GHC
import qualified TcEnv as GHC
import qualified TcEvidence as GHC
import qualified TcExpr as GHC
import qualified TcHsSyn as GHC
import qualified TcRnMonad as GHC
import qualified TcSMonad as GHC ( runTcS )
import qualified TcSimplify as GHC
import qualified TcType as GHC

-- syb
import Data.Generics ( everywhereM, mkM )

-- template-haskell
import Language.Haskell.TH as TH


plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.typeCheckResultAction = \_cliOptions -> pluginImpl
    }


pluginImpl :: GHC.ModSummary -> GHC.TcGblEnv -> GHC.TcM GHC.TcGblEnv
pluginImpl _modSummary tcGblEnv = do 
  hscEnv <-
    GHC.getTopEnv

  GHC.Found _ assertExplainerModule <-
    liftIO
      ( GHC.findImportedModule
          hscEnv
          ( GHC.mkModuleName "WhatItDo" )
          Nothing
      )

  traceDoName <-
    GHC.lookupId
      =<< GHC.lookupOrig assertExplainerModule ( GHC.mkVarOcc "traceDo" )

  tcg_binds <-
    mkM ( whatItDo traceDoName ) `everywhereM` GHC.tcg_binds tcGblEnv

  return tcGblEnv { GHC.tcg_binds = tcg_binds }


traceDo :: m a -> m a
traceDo =
  id


whatItDo
  :: GHC.Id -> Expr.LHsExpr GHC.GhcTc -> GHC.TcM ( Expr.LHsExpr GHC.GhcTc )
whatItDo traceDoName ( GHC.L _ ( Expr.HsApp _ ( GHC.L _ ( Expr.HsWrap _ _ ( Expr.HsVar _ ( GHC.L _ v ) ) ) ) ( GHC.L _ body ) ) ) | v == traceDoName = do
  GHC.noLoc <$> go body

  where

    go ( Expr.HsDo t Expr.DoExpr ( GHC.L _ stmts ) ) = do
      stmts' <-
        for
          stmts
          ( \orig@( GHC.L loc stmt ) ->
              case stmt of
                Expr.BindStmt bt pat expr e1 e2 -> do
                  traceBind loc bt pat expr e1 e2
  
                _ ->
                  return orig
          )
  
      return ( Expr.HsDo t Expr.DoExpr ( GHC.noLoc stmts' ) )
  
    go ( Expr.HsPar a ( GHC.L b c ) ) =
      Expr.HsPar a <$> ( GHC.L b <$> go c )

    go o@( Expr.HsVar _ ( GHC.L loc _ ) ) =
      o
        <$ GHC.addWarnAt
             GHC.NoReason
             loc
             ( PP.text "debugDo cannot be used on variables" )

whatItDo _ other =
  return other


traceBind
  :: GHC.SrcSpan
  -> GHC.Type
  -> GHC.LPat GHC.GhcTc
  -> Expr.LHsExpr GHC.GhcTc
  -> Expr.SyntaxExpr GHC.GhcTc
  -> Expr.SyntaxExpr GHC.GhcTc
  -> GHC.TcM ( Expr.LStmt GHC.GhcTc ( Expr.LHsExpr GHC.GhcTc ) )
traceBind loc t pat expr e1 e2 = do
  Just exprT <-
    typeOfExpr expr 

  let
    ( _m, a ) =
      GHC.splitAppTy exprT

  canShow <-
    hasShow a

  if canShow
    then do
      let
        ppWhere =
          GHC.renderWithStyle
            GHC.unsafeGlobalDynFlags
            ( GHC.ppr loc )
            ( GHC.defaultUserStyle GHC.unsafeGlobalDynFlags )

        ppPat =
          GHC.renderWithStyle
            GHC.unsafeGlobalDynFlags
            ( GHC.ppr pat )
            ( GHC.defaultUserStyle GHC.unsafeGlobalDynFlags )
      
      Right traceExprPs <-
        fmap ( GHC.convertToHsExpr GHC.noSrcSpan )
          $ liftIO
          $ TH.runQ
          $ [| ( >>= \x ->
                   trace
                     ( "("
                         ++ ppWhere
                         ++ ") "
                         ++ ppPat
                         ++ " = "
                         ++ show x
                     )
                     ( return x )
               )
            |]

      ( traceExprRn, _ ) <-
        GHC.rnLExpr traceExprPs 

      ( traceExprTc, wanteds ) <-
        GHC.captureConstraints
          ( GHC.tcMonoExpr
              traceExprRn
              ( GHC.Check ( GHC.mkFunTy exprT exprT ) )
          )

      -- Solve wanted constraints and build a wrapper.
      evBinds <-
        GHC.EvBinds . GHC.evBindMapBinds . snd
          <$> GHC.runTcS ( GHC.solveWanteds wanteds )

      ( _, zonkedEvBinds ) <-
        GHC.zonkTcEvBinds GHC.emptyZonkEnv evBinds

      let
        wrapper =
          GHC.mkWpLet zonkedEvBinds

      -- Apply the wrapper to our type checked syntax and fully saturate the
      -- diagnostic function with the necessary arguments.
      newBody <-
        GHC.zonkTopLExpr
          ( GHC.mkHsApp 
              ( GHC.mkLHsWrap wrapper traceExprTc )
              expr
          )

      return ( GHC.L loc ( Expr.BindStmt t pat newBody e1 e2 ) )
  else
    return ( GHC.L loc ( Expr.BindStmt t pat expr e1 e2 ) )


typeOfExpr :: Expr.LHsExpr GHC.GhcTc -> GHC.TcM ( Maybe GHC.Type )
typeOfExpr e = do
  hs_env  <-
    GHC.getTopEnv

  ( _, mbe ) <-
    liftIO ( GHC.deSugarExpr hs_env e )

  return ( CoreUtils.exprType <$> mbe )
  


-- | Given a typed expression, ensure that it has a Show instance.
hasShow :: GHC.Type -> GHC.TcM Bool
hasShow t = do
  showTyCon <-
    GHC.tcLookupTyCon GHC.showClassName

  dictName <-
    GHC.newName ( GHC.mkDictOcc ( GHC.mkVarOcc "magic" ) )

  let
    dict_ty =
      GHC.mkTyConApp showTyCon [ t ]

    dict_var =
      GHC.mkVanillaGlobal dictName dict_ty

  GHC.EvBinds evBinds <-
    Constraint.getDictionaryBindings dict_var dict_ty

  return ( not ( null ( toList evBinds ) ) )
