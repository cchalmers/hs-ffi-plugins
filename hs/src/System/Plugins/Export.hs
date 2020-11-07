{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE UnboxedTuples            #-}
module System.Plugins.Export where

import Data.Maybe (fromMaybe)
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.Data              as Data
import           Data.Dynamic
import           Data.IORef
import           Data.Kind              (Type)
import           Data.Typeable          (Proxy (..), typeRepFingerprint)
import           Data.Word
import           Foreign
import           Foreign.C              (CString, peekCString)
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable
import           GHC.Fingerprint.Type
import           System.IO.Unsafe
import           Type.Reflection
import           Unsafe.Coerce

import qualified BasicTypes
import           BinIface
import           DynFlags               (defaultDynFlags, initDynFlags)
import qualified DynFlags
import qualified GHC
import           GHC.Exts
import           GHC.Paths              (libdir)
import qualified GHC.Types              as Prim
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
import qualified Packages

-- foreign import ccall "dynamic" ioNext :: FunPtr (Ptr () -> IO Word64) -> Ptr () -> IO Word64
-- foreign import ccall "dynamic" iofd :: FunPtr (Ptr () -> IO Bool) -> Ptr () -> IO Bool

foreign import ccall "dynamic" ffiNextUnit ::
  FunPtr (Ptr () -> Ptr () -> IO Bool)
       -> Ptr () -> Ptr () -> IO Bool

ffiNext ::
  FunPtr (Ptr () -> Ptr a -> IO Bool)
       -> Ptr () -> Ptr a -> IO Bool
ffiNext fptr stb a = ffiNextUnit (castFunPtr fptr) stb (castPtr a)

foreign import ccall "dynamic" ffiFree ::
  FunPtr (Ptr () -> IO ()) -> Ptr () -> IO ()

stableRef :: a -> IO (StablePtr (IORef a))
stableRef a = newIORef a >>= newStablePtr

foreignList
  :: Storable a
  => FunPtr (Ptr () -> Ptr a -> IO Bool)
  -> FunPtr (Ptr () -> IO ())
  -> Ptr ()
  -> IO [a]
foreignList next free ctx = do
  let iom = alloca $ \w -> do
        ffiNext next ctx w >>= \case
          True -> Just <$> Foreign.peek w
          False -> ffiFree free ctx >> pure Nothing
  lazyIOM iom

foreignListRef
  :: Storable a
  => FunPtr (Ptr () -> Ptr a -> IO Bool)
  -> FunPtr (Ptr () -> IO ())
  -> Ptr ()
  -> IO (StablePtr (IORef [a]))
foreignListRef next free ctx = do
  foreignList next free ctx >>= stableRef

-- | Clone a stable pointer to an ioref list.
cloneStableRef
  :: StablePtr (IORef [Prim.Any])
  -> IO (StablePtr (IORef [Prim.Any]))
cloneStableRef ptr = do
  ioref <- deRefStablePtr ptr
  xs <- readIORef ioref
  ioref' <- newIORef xs
  newStablePtr ioref'

-- | Free a stable pointer
freeStable
  :: StablePtr ()
  -> IO ()
freeStable ptr = do
  freeStablePtr ptr

lazyIOM :: IO (Maybe a) -> IO [a]
lazyIOM iow = go
  where
    go = do
      -- putStrLn "doing a step of go"
      unsafeInterleaveIO iow >>= \case
        Nothing -> pure []
        Just x  -> do
          xs <- unsafeInterleaveIO go
          pure (x:xs)

lazyIO :: IO a -> IO [a]
lazyIO iow = go
  where
    go = do
      -- putStrLn "doing a step of go"
      x <- unsafeInterleaveIO iow
      -- putStrLn "got that x"
      xs <- unsafeInterleaveIO go
      pure (x:xs)

-- tester :: FunPtr (Ptr () -> IO Word64) -> Ptr () -> IO ()
-- tester fptr ctx = do
--   let f = ioNext fptr ctx
--   iows <- lazyIO f
--   print (take 10 iows)

hello :: IO ()
hello = putStrLn "Hello"

myList :: [Word64]
myList = [1..]

mkNewList :: IO (StablePtr (IORef [Word64]))
mkNewList = newIORef myList >>= newStablePtr

mk_list_iter :: StablePtr [Any] -> IO (StablePtr (IORef [Any]))
mk_list_iter ptr = deRefStablePtr ptr >>= newIORef >>= newStablePtr

nextList :: Storable a => StablePtr (IORef [a]) -> Ptr a -> IO Bool
nextList list ptr = do
  ioref <- deRefStablePtr list
  readIORef ioref >>= \case
    []   -> pure False
    x:xs -> do
      writeIORef ioref xs
      Foreign.poke ptr x
      pure True

type ForeignListType a
   = FunPtr (Ptr () -> Ptr a -> IO Bool)
  -> FunPtr (Ptr () -> IO ())
  -> Ptr ()
  -> IO (StablePtr (IORef [a]))

foreignListRefU64 :: ForeignListType Word64
foreignListRefU64 = foreignListRef
foreignListRefU32 :: ForeignListType Word32
foreignListRefU32 = foreignListRef
foreignListRefU16 :: ForeignListType Word16
foreignListRefU16 = foreignListRef
foreignListRefU8  :: ForeignListType Word8
foreignListRefU8  = foreignListRef
foreignListRefI64 :: ForeignListType Int64
foreignListRefI64 = foreignListRef
foreignListRefI32 :: ForeignListType Int32
foreignListRefI32 = foreignListRef
foreignListRefI16 :: ForeignListType Int16
foreignListRefI16 = foreignListRef
foreignListRefI8  :: ForeignListType Int8
foreignListRefI8  = foreignListRef

foreign export ccall foreignListRefU64 :: ForeignListType Word64
foreign export ccall foreignListRefU32 :: ForeignListType Word32
foreign export ccall foreignListRefU16 :: ForeignListType Word16
foreign export ccall foreignListRefU8  :: ForeignListType Word8
foreign export ccall foreignListRefI64 :: ForeignListType Int64
foreign export ccall foreignListRefI32 :: ForeignListType Int32
foreign export ccall foreignListRefI16 :: ForeignListType Int16
foreign export ccall foreignListRefI8  :: ForeignListType Int8
-- foreign export ccall foreignListRefU8 :: ForeignListType Bool

-- nextList8 :: StablePtr (IORef [Word8]) -> Ptr Word8 -> IO Bool
-- nextList8 = nextList

nextList8 :: StablePtr (IORef [Word8]) -> Ptr Word8 -> IO Bool
nextList8 = nextList
nextList16 :: StablePtr (IORef [Word16]) -> Ptr Word16 -> IO Bool
nextList16 = nextList
nextList32 :: StablePtr (IORef [Word32]) -> Ptr Word32 -> IO Bool
nextList32 = nextList
nextList64 :: StablePtr (IORef [Word64]) -> Ptr Word64 -> IO Bool
nextList64 = nextList
-- nextListBool :: StablePtr (IORef [Bool]) -> Ptr Bool -> IO Bool
-- nextListBool = nextList

loadloadload :: IO ()
loadloadload = pure ()

foreign export ccall hello :: IO ()
foreign export ccall mkNewList :: IO (StablePtr (IORef [Word64]))
foreign export ccall mk_list_iter :: StablePtr [Any] -> IO (StablePtr (IORef [Any]))
foreign export ccall cloneStableRef
  :: StablePtr (IORef [Prim.Any])
  -> IO (StablePtr (IORef [Prim.Any]))

foreign export ccall freeStable :: StablePtr () -> IO ()

-- foreign export ccall nextListBool :: StablePtr (IORef [Bool]) -> Ptr Bool -> IO Bool
foreign export ccall nextList8 :: StablePtr (IORef [Word8]) -> Ptr Word8 -> IO Bool
foreign export ccall nextList16 :: StablePtr (IORef [Word16]) -> Ptr Word16 -> IO Bool
foreign export ccall nextList32 :: StablePtr (IORef [Word32]) -> Ptr Word32 -> IO Bool
foreign export ccall nextList64 :: StablePtr (IORef [Word64]) -> Ptr Word64 -> IO Bool

foreign export ccall loadloadload :: IO ()

deref_word64 :: StablePtr Word64 -> IO Word64
deref_word64 = deRefStablePtr

deref_int64 :: StablePtr Int64 -> IO Int64
deref_int64 = deRefStablePtr

foreign export ccall deref_word64 :: StablePtr Word64 -> IO Word64
foreign export ccall deref_int64 :: StablePtr Int64 -> IO Int64

-- ghci like interface


-- | Create a new initialised session.
unsafeNewSession :: Maybe String -> IO Session
unsafeNewSession lib = do
  putStrLn $ "using lib " <> show lib
  ref <- newIORef (error "empty session")
  let session = Session ref
  flip unGhc session $ GHC.initGhcMonad (Just $ fromMaybe libdir lib)
  linkInMemory session -- TEMP
  pure session

sessionFlags :: Session -> IO GHC.DynFlags
sessionFlags session = flip unGhc session $ do
  GHC.getSessionDynFlags

linkInMemory :: Session -> IO ()
linkInMemory session = flip unGhc session $ do
  dflags <- GHC.getSessionDynFlags
  liftIO $ Packages.initPackages dflags
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
  session <- unsafeNewSession Nothing
  importModules session ["Prelude", "Plug2"]
  res <- compExpr session expr
  either print (\val -> print (unsafeCoerce val :: [Int])) res
  -- print is
  cleanup session

-- typereps ------------------------------------------------------------

int_type_rep :: IO (StablePtr SomeTypeRep)
int_type_rep = newStablePtr $ someTypeRep (Proxy @Int)

word_type_rep :: IO (StablePtr SomeTypeRep)
word_type_rep = newStablePtr $ someTypeRep (Proxy @Word)

int64_type_rep :: IO (StablePtr SomeTypeRep)
int64_type_rep = newStablePtr $ someTypeRep (Proxy @Int64)

word64_type_rep :: IO (StablePtr SomeTypeRep)
word64_type_rep = newStablePtr $ someTypeRep (Proxy @Word64)

list_type_rep :: StablePtr SomeTypeRep -> IO (StablePtr SomeTypeRep)
list_type_rep aTyPtr = do
  SomeTypeRep (aTy :: TypeRep a) <- deRefStablePtr aTyPtr
  case eqTypeRep (typeRepKind aTy) (typeRep @Type) of
    Nothing    -> error "That wasn't a Type TypeRep!"
    Just HRefl -> newStablePtr $ withTypeable aTy (someTypeRep (Proxy @[a]))

type_rep_fingerprint :: StablePtr SomeTypeRep -> Ptr Word64 -> Ptr Word64 -> IO ()
type_rep_fingerprint tyPtr aPtr bPtr = do
  typeRep <- deRefStablePtr tyPtr
  let Fingerprint a b = typeRepFingerprint typeRep
  poke aPtr a
  poke bPtr b

dyn_type_rep :: StablePtr Dynamic -> IO (StablePtr SomeTypeRep)
dyn_type_rep dynPtr = do
  dyn <- deRefStablePtr dynPtr
  newStablePtr $ dynTypeRep dyn

dyn_val :: StablePtr Dynamic -> IO (StablePtr Any)
dyn_val dynPtr = do
  Dynamic rep a <- deRefStablePtr dynPtr
  newStablePtr $ unsafeCoerce a

foreign export ccall int_type_rep :: IO (StablePtr SomeTypeRep)
foreign export ccall word_type_rep :: IO (StablePtr SomeTypeRep)
foreign export ccall int64_type_rep :: IO (StablePtr SomeTypeRep)
foreign export ccall word64_type_rep :: IO (StablePtr SomeTypeRep)
foreign export ccall list_type_rep :: StablePtr SomeTypeRep -> IO (StablePtr SomeTypeRep)
foreign export ccall type_rep_fingerprint :: StablePtr SomeTypeRep -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign export ccall dyn_type_rep :: StablePtr Dynamic -> IO (StablePtr SomeTypeRep)
foreign export ccall dyn_val :: StablePtr Dynamic -> IO (StablePtr Any)

-- ghc api -------------------------------------------------------------

new_session :: CString -> IO (StablePtr Session)
new_session cstr = do
  print cstr
  str <- peekCString cstr
  print str
  lib >>= unsafeNewSession >>= newStablePtr
  where lib = if cstr == nullPtr then pure Nothing else Just <$> (peekCString cstr)

run_expr :: StablePtr Session -> CString -> IO ()
run_expr ptr cexpr = do
  session <- deRefStablePtr ptr
  expr <- peekCString cexpr
  compExpr session ("print (" <> expr <> ") :: IO ()") >>= \case
    Right io -> unsafeCoerce io
    Left err -> print err

run_expr_dyn :: StablePtr Session -> CString -> Ptr (StablePtr Dynamic) -> IO Word64
run_expr_dyn ptr cexpr ptrPtr = do
  session <- deRefStablePtr ptr
  expr <- peekCString cexpr
  compExprDyn session expr >>= \case
    Right dyn -> do
      newStablePtr dyn >>= poke ptrPtr
      pure 1
    Left err -> do
      print err
      pure 0

cleanup_session :: StablePtr Session -> IO ()
cleanup_session ptr = do
  sess <- deRefStablePtr ptr
  cleanup sess

import_modules :: StablePtr Session -> Int -> Ptr CString -> IO ()
import_modules ptr n cmods = do
  session <- deRefStablePtr ptr
  mods <- mapM (\n -> peekElemOff cmods n >>= peekCString) [0..n-1]
  importModules session mods

debugging :: StablePtr Session -> IO ()
debugging ptr = do
  session <- deRefStablePtr ptr
  flags <- sessionFlags session
  let pdb = GHC.pkgDatabase flags
  putStrLn $ "the package database is " <> show (fmap (map fst) pdb)

foreign export ccall new_session :: CString -> IO (StablePtr Session)
foreign export ccall run_expr :: StablePtr Session -> CString -> IO ()
foreign export ccall run_expr_dyn :: StablePtr Session -> CString -> Ptr (StablePtr Dynamic) -> IO Word64
foreign export ccall cleanup_session :: StablePtr Session -> IO ()
foreign export ccall import_modules :: StablePtr Session -> Int -> Ptr CString -> IO ()
foreign export ccall debugging :: StablePtr Session -> IO ()
