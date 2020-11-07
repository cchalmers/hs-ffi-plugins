{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE UnboxedTuples            #-}

{-# OPTIONS -Wno-orphans #-}

module Plugs where

import qualified BasicTypes
import           BinIface
import           DynFlags               (defaultDynFlags, initDynFlags)
import qualified DynFlags
import qualified GHC
import qualified GHCi.Message           as GHCi
import           GhcMonad               (Ghc (..), Session (..))
import           HscMain                (newHscEnv)
import           HscTypes
import           IfaceSyn
import           Module
import           Module                 (moduleName, moduleNameString)
import           Name
import           Outputable             hiding ((<>))
import           SysTools               (initLlvmConfig, initSysTools)
import           TcRnMonad              (initTcRnIf)

import           GHC.Paths              (libdir)

import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.Data              as Data
import           Data.Dynamic
import           Data.IORef
import           Data.Typeable
import           Foreign.C              (CString, peekCString)
import           Foreign.StablePtr
import           Unsafe.Coerce

-- | Create a new initialised session.
unsafeNewSession :: IO Session
unsafeNewSession = do
  ref <- newIORef (error "empty session")
  let session = Session ref
  flip unGhc session $ GHC.initGhcMonad (Just libdir)
  linkInMemory session -- TEMP
  GHC.load GHC.LoadAllTargets >>= liftIO . print -- TEMP
  pure session

linkInMemory :: Session -> IO ()
linkInMemory session = flip unGhc session $ do
  dflags <- GHC.getSessionDynFlags
  -- returns list of new packages that may need to be linked, unsure
  _ <- GHC.setSessionDynFlags dflags
    { DynFlags.hscTarget = DynFlags.HscAsm
    , DynFlags.ghcLink   = DynFlags.LinkInMemory
    }
  pure ()

importModules :: Session -> [String] -> IO ()
importModules session modules = flip unGhc session $ do
  GHC.setContext $ map (GHC.IIDecl . GHC.simpleImportDecl . mkModuleName) modules

compExpr :: Session -> String -> IO (Either GHCi.SerializableException GHC.HValue)
compExpr session expr = tryJust (Just . GHCi.toSerializableException) $ flip unGhc session $ do
  GHC.compileExpr expr

compExprDyn :: Session -> String -> IO (Either GHCi.SerializableException Dynamic)
compExprDyn session expr = tryJust (Just . GHCi.toSerializableException) $ flip unGhc session $ do
  GHC.dynCompileExpr expr

cleanup :: Session -> IO ()
cleanup session = flip unGhc session $ GHC.withCleanupSession (pure ())

rrr :: String -> IO ()
rrr expr = do
  session <- unsafeNewSession
  importModules session ["Prelude", "Plug2"]
  res <- compExpr session expr
  either print (\val -> print (unsafeCoerce val :: [Int])) res
  -- print is
  cleanup session

-- c ffi ---------------------------------------------------------------

new_session :: IO (StablePtr Session)
new_session = unsafeNewSession >>= newStablePtr

run_expr :: StablePtr Session -> CString -> IO ()
run_expr ptr cexpr = do
  session <- deRefStablePtr ptr
  expr <- peekCString cexpr
  compExpr session ("print (" <> expr <> ") :: IO ()") >>= \case
    Right io -> unsafeCoerce io
    Left err -> print err


cleanup_session :: StablePtr Session -> IO ()
cleanup_session ptr = do
  sess <- deRefStablePtr ptr
  cleanup sess

import_module :: StablePtr Session -> CString -> IO ()
import_module ptr cmod = do
  session <- deRefStablePtr ptr
  modu <- peekCString cmod
  importModules session [modu]

foreign export ccall new_session :: IO (StablePtr Session)
foreign export ccall run_expr :: StablePtr Session -> CString -> IO ()
foreign export ccall cleanup_session :: StablePtr Session -> IO ()
foreign export ccall import_module :: StablePtr Session -> CString -> IO ()

-- experimenting -------------------------------------------------------


-- load

ifaceModuleName :: ModIface -> String
ifaceModuleName = moduleNameString . moduleName . mi_module

withSession' :: GHC.Ghc a -> IO a
withSession' action =
    GHC.defaultErrorHandler DynFlags.defaultFatalMessager DynFlags.defaultFlushOut $ do
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        _ <- GHC.setSessionDynFlags dflags
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

  -- GHC.load GHC.LoadAllTargets >>= liftIO . print
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
      IfVanillaId                     -> putStrLn "IfVanillaId"
      IfRecSelId _eitherConDecl _bool -> putStrLn "IfRecSelId"
      IfDFunId                        -> putStrLn "IfDFunId"
    case inf of
      NoInfo       -> putStrLn "NoInfo"
      HasInfo info -> putStrLn $ "HasInfo " <> show (length info)
    putStrLn ""
  _ -> putStrLn "Unknown delc"

showIfaceArgs :: IfaceTcArgs -> String
showIfaceArgs = \case
  ITC_Nil -> "ITC_Nil"
  ITC_Vis ifaceType _ifaceTcArgs -> "ITC_VIS (" <> showIfaceTy ifaceType <> ")"
  ITC_Invis _ifaceKind _ifaceTcArgs -> "ITC_INVIS"

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
  IfaceFreeTyVar _tyVar -> "<free ty var>"
  IfaceTyVar _ifLclName -> "<ty var>"
  IfaceLitTy _ifaceTyLit -> "ty lit"
  IfaceAppTy a b -> showIfaceTy a <> " `appTy` " <> showIfaceTy b
  IfaceFunTy a b -> showIfaceTy a <> " `funTy` " <> showIfaceTy b
  IfaceDFunTy a b -> showIfaceTy a <> " `dFunTy` " <> showIfaceTy b
  IfaceForAllTy _ifaceForAllBndr a -> "forall " <> showIfaceTy a
  IfaceTyConApp (IfaceTyCon ifExtName (IfaceTyConInfo isPromoted ifaceTyConSort_)) ifaceTcArgs ->
    "ifExtName is " <> getOccString ifExtName <> " "
      <> show isPromoted <> " "
      <> show ifaceTyConSort_ <> " \n  ->\n  "
      <> showIfaceArgs ifaceTcArgs
  IfaceCastTy _ifaceType _ifaceCoercion -> "cast"
  IfaceCoercionTy _ifaceCoercion -> "coersion"
  IfaceTupleTy _basicTypes_tupleSort _isPromoted _ifaceTcArgs -> "tuple"


pp :: Outputable a => a -> IO ()
pp a = GHC.runGhc (Just libdir) $ do
  dflags <- DynFlags.getDynFlags
  liftIO $ putStrLn (showPpr dflags a)

showC :: Data.Data a => a -> String
showC a = show (typeOf a) <> " " <> show (Data.toConstr a)
