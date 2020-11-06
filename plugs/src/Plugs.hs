{-# LANGUAGE CPP                        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
--
-- Copyright (C) 2004-5 Don Stewart
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
-- USA
--

-- | An interface to the GHC runtime's dynamic linker, providing runtime
-- loading and linking of Haskell object files, commonly known as
-- /plugins/.

module Plugs (

      -- -- * The @LoadStatus@ type
      -- LoadStatus(..)

      -- -- * High-level interface
      -- , load
      -- , load_
      -- , dynload
      -- , pdynload
      -- , pdynload_
      -- , unload
      -- , unloadAll
      -- , reload
      -- , Module(..)

      -- -- * Low-level interface
      -- , initLinker      -- start it up
      -- , loadModule      -- load a vanilla .o
      -- , loadFunction    -- retrieve a function from an object
      -- , loadFunction_   -- retrieve a function from an object
      -- , loadPackageFunction
      -- , loadPackage     -- load a ghc library and its cbits
      -- , unloadPackage   -- unload a ghc library and its cbits
      -- , loadPackageWith -- load a pkg using the package.conf provided
      -- , loadShared      -- load a .so object file
      -- , resolveObjs     -- and resolve symbols

      -- , loadRawObject   -- load a bare .o. no dep chasing, no .hi file reading

      -- , Symbol

      -- , getImports

  readBinIface',
xxx
  ) where

-- #include "config.h"

-- import System.Plugins.Make             ( build )
-- import System.Plugins.Env
-- import System.Plugins.Utils
-- import System.Plugins.Consts           ( sysPkgSuffix, hiSuf, prefixUnderscore )
-- import System.Plugins.LoadTypes

-- import Language.Hi.Parser
import Encoding (zEncodeString)
import BinIface
import HscTypes
import qualified BasicTypes

import Module (moduleName, moduleNameString)
-- #if MIN_VERSION_ghc(8,0,0)
-- #if MIN_VERSION_Cabal(2,0,0)
-- import Module (installedUnitIdString)
-- #else
-- import Module (unitIdString)
-- #endif
-- #elif MIN_VERSION_ghc(7,10,0)
-- import Module (packageKeyString)
-- #else
-- import Module (packageIdString)
-- #endif

import HscMain (newHscEnv)
import TcRnMonad (initTcRnIf)

import Data.Dynamic          ( fromDynamic, Dynamic )
import Data.Typeable         ( Typeable )

import Data.List                ( isSuffixOf, nub, nubBy )
import Control.Monad            ( when, filterM, liftM )
-- import System.Directory         ( doesFileExist, removeFile )
import Foreign.C                ( CInt(..) )
import Foreign.C.String         ( CString, withCString, peekCString )

-- #if !MIN_VERSION_ghc(7,2,0)
-- import GHC                      ( defaultCallbacks )
-- #else
import DynFlags                 (defaultDynFlags, initDynFlags)
import GHC.Paths                (libdir)
import SysTools                 (initSysTools, initLlvmConfig)
-- #endif
import GHC.Ptr                  ( Ptr(..), nullPtr )
-- #if !MIN_VERSION_ghc(7,4,1)
-- import GHC.Exts                 ( addrToHValue# )
-- #else
import GHC.Exts                 ( addrToAny# )
-- #endif

import GHC.Prim                 ( unsafeCoerce# )

#if DEBUG
import System.IO                ( hFlush, stdout )
#endif
import System.IO                ( hClose )

import qualified Data.Data as Data
import Data.Typeable

import Outputable hiding ((<>))
import Name
import IfaceSyn
import Module
import qualified GHC
import qualified DynFlags

import Control.Monad.IO.Class

import Unsafe.Coerce

ifaceModuleName = moduleNameString . moduleName . mi_module

withSession' :: GHC.Ghc a -> IO a
withSession' action =
    GHC.defaultErrorHandler DynFlags.defaultFatalMessager DynFlags.defaultFlushOut $ do
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        GHC.setSessionDynFlags dflags
          { DynFlags.hscTarget = DynFlags.HscAsm
          , DynFlags.ghcLink   = DynFlags.LinkInMemory
          }
        action

moduleTarget :: String -> GHC.Target
moduleTarget name = GHC.Target
  { GHC.targetId = GHC.TargetModule (GHC.mkModuleName name)
  , GHC.targetAllowObjCode = False
  , GHC.targetContents = Nothing
  }

objTarget :: String -> GHC.Target
objTarget path = GHC.Target
  { GHC.targetId = GHC.TargetFile path Nothing
  , GHC.targetAllowObjCode = True
  , GHC.targetContents = Nothing
  }

deriving instance Show GHC.SuccessFlag

xxx :: IO ()
xxx = withSession' $ do
-- xxx = GHC.runGhc (Just libdir) $ do
  expr <- GHC.parseExpr "1 + 2"
  dflags <- DynFlags.getDynFlags
  liftIO $ putStrLn (showPpr dflags expr)
  liftIO $ putStrLn "HI"

  -- GHC.addTarget (moduleTarget "Prelude")
  -- GHC.addTarget (moduleTarget "Plug2")

  -- XXX This part doesn't work when run in `cabal repl`, it complains about a missing "Plug.dyn_o".
  -- I can compile separately with `ghc --make --dynamic-too Plug.hs` but then it complains about
  -- weird misspelt packages like: `ghc-pths-0.1.0.12-5d9fa1db`. Also the path get included in this
  -- packages conf file so it looks like it's getting confused. Running with `cabal run` seems to
  -- work fine.
  -- GHC.addTarget (objTarget "../dyn/Plug.hs")

  -- Anyway, loading like this probably isn't what I want, just having a precompiled package in scope
  -- means I can load the module normally.

  GHC.load GHC.LoadAllTargets >>= liftIO . print
  GHC.setContext (map (GHC.IIDecl . GHC.simpleImportDecl)
    [mkModuleName "Prelude", mkModuleName "Plug2"])

  liftIO $ putStrLn "HI"
  -- val <- GHC.compileExpr "take 3 Plug.myCoolList"
  val <- GHC.compileExpr "take 3 myCoolList"
  let val2 = unsafeCoerce val :: [Int]
  liftIO $ print val2
  -- val3 <- GHC.compileExpr "take 3 Plug.myCoolList"
  -- let val4 = unsafeCoerce val3 :: [Int]
  -- liftIO $ print val4
  pure ()

readBinIface' :: FilePath -> IO ModIface
readBinIface' hi_path = do
  mySettings <- initSysTools (Just libdir) -- how should we really set the top dir?
  llvmConfig <- initLlvmConfig (Just libdir)
  dflags <- initDynFlags (defaultDynFlags mySettings llvmConfig)
  e <- newHscEnv dflags
  initTcRnIf 'r' e undefined undefined (readBinIface IgnoreHiWay QuietBinIFaceReading hi_path)

myCoolDecls :: IO [IfaceDecl]
myCoolDecls = do
  miface <- readBinIface' "../dyn/Plug.hi"
  pure $ map snd (mi_decls miface)

myCoolDecl :: IO IfaceDecl
myCoolDecl = do
  miface <- readBinIface' "../dyn/Plug.hi"
  let decls = mi_decls miface
  let [_, _, (_, myCoolIface)] = decls
  pure myCoolIface

declInfo :: IfaceDecl -> IO ()
declInfo = \case
  IfaceId name ty deets inf -> do
    putStrLn $ "name: " <> getOccString name
    pp ty
    -- putStrLn $ showIfaceTy ty
    case deets of
      IfVanillaId -> putStrLn "IfVanillaId"
      IfRecSelId eitherConDecl bool -> putStrLn "IfRecSelId"
      IfDFunId -> putStrLn "IfDFunId"
    case inf of
      NoInfo -> putStrLn "NoInfo"
      HasInfo info -> putStrLn $ "HasInfo " <> show (length info)
    putStrLn ""
  _ -> putStrLn "Unknown delc"

showIfaceArgs :: IfaceTcArgs -> String
showIfaceArgs = \case
  ITC_Nil -> "ITC_Nil"
  ITC_Vis ifaceType ifaceTcArgs -> "ITC_VIS (" <> showIfaceTy ifaceType <> ")"
  ITC_Invis ifaceKind ifaceTcArgs -> "ITC_INVIS"

deriving instance Show IsPromoted
deriving instance Show BasicTypes.TupleSort
deriving instance Show IfaceTyConSort

-- showIfaceTyConSort :: IfaceTyConSort -> String
-- showIfaceTyConSort = \case
--   IfaceNormalTyCon -> "IfaceNormalTyCon "
--   IfaceTupleTyCon {-# UNPACK #-}BasicTypes.Arity
--                   !BasicTypes.TupleSort
--   IfaceSumTyCon {-# UNPACK #-}BasicTypes.Arity
--   IfaceEqualityTyCon


showIfaceTy :: IfaceType -> String
showIfaceTy = \case
  IfaceFreeTyVar tyVar -> "<free ty var>"
  IfaceTyVar ifLclName -> "<ty var>"
  IfaceLitTy ifaceTyLit -> "ty lit"
  IfaceAppTy a b -> showIfaceTy a <> " `appTy` " <> showIfaceTy b
  IfaceFunTy a b -> showIfaceTy a <> " `funTy` " <> showIfaceTy b
  IfaceDFunTy a b -> showIfaceTy a <> " `dFunTy` " <> showIfaceTy b
  IfaceForAllTy ifaceForAllBndr a -> "forall " <> showIfaceTy a
  IfaceTyConApp (IfaceTyCon ifExtName (IfaceTyConInfo isPromoted ifaceTyConSort)) ifaceTcArgs ->
    "ifExtName is " <> getOccString ifExtName <> " "
      <> show isPromoted <> " "
      <> show ifaceTyConSort <> " \n  ->\n  "
      <> showIfaceArgs ifaceTcArgs
  IfaceCastTy ifaceType ifaceCoercion -> "cast"
  IfaceCoercionTy ifaceCoercion -> "coersion"
  IfaceTupleTy basicTypes_tupleSort isPromoted ifaceTcArgs -> "tuple"


pp :: Outputable a => a -> IO ()
pp a = GHC.runGhc (Just libdir) $ do
  dflags <- DynFlags.getDynFlags
  liftIO $ putStrLn (showPpr dflags a)

showC :: Data.Data a => a -> String
showC a = show (typeOf a) <> " " <> show (Data.toConstr a)
