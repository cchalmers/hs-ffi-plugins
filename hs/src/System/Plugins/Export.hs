{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE UnboxedTuples            #-}
module System.Plugins.Export where

import           Data.ByteString.Internal

import           Control.Concurrent
import           Control.Exception
import           Control.Monad            (filterM, forM_)
import           Control.Monad.IO.Class
import qualified Data.Data                as Data
import           Data.Dynamic
import           Data.Graph               (flattenSCCs)
import           Data.IORef
import           Data.Kind                (Type)
import           Data.Maybe               (fromMaybe)
import           Data.Typeable            (Proxy (..), typeRepFingerprint)
import           Data.Word
import           Foreign
import           Foreign.C                (CString, peekCString)
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

import           System.IO                as IO

import qualified BasicTypes
import           BinIface
import           DynFlags                 (HasDynFlags, defaultDynFlags,
                                           initDynFlags)
import qualified DynFlags
import           Exception                (ExceptionMonad, gmask)
import qualified GHC
import           GHC.Exts
import           GHC.Paths                (libdir)
import qualified GHC.Types                as Prim
import qualified GHCi.Message             as GHCi
import           GhcMonad                 (Ghc (..), Session (..))
import           HscMain                  (newHscEnv)
import           HscTypes
import           IfaceSyn
import           Module
import           Module                   (moduleName, moduleNameString)
import           Name
import           Outputable               hiding ((<>))
import qualified Packages
import           SysTools                 (initLlvmConfig, initSysTools)
import           TcRnMonad                (initTcRnIf)

import           Debug.Trace

-- foreign import ccall "dynamic" ioNext :: FunPtr (Ptr () -> IO Word64) -> Ptr () -> IO Word64
-- foreign import ccall "dynamic" iofd :: FunPtr (Ptr () -> IO Bool) -> Ptr () -> IO Bool

foreign import ccall "dynamic" ffiNextUnit ::
  FunPtr (Ptr () -> Ptr () -> IO Word8)
       -> Ptr () -> Ptr () -> IO Word8

ffiNext ::
  FunPtr (Ptr () -> Ptr a -> IO Word8)
       -> Ptr () -> Ptr a -> IO Word8
ffiNext fptr stb a = ffiNextUnit (castFunPtr fptr) stb (castPtr a)

foreign import ccall "dynamic" ffiFree ::
  FunPtr (Ptr () -> IO ()) -> Ptr () -> IO ()

stableRef :: a -> IO (StablePtr (IORef a))
stableRef a = newIORef a >>= newStablePtr

foreignList
  :: Storable a
  => FunPtr (Ptr () -> Ptr a -> IO Word8)
  -> FunPtr (Ptr () -> IO ())
  -> Ptr ()
  -> IO [a]
foreignList next free ctx = do
  let iom = alloca $ \w -> do
        ffiNext next ctx w >>= \case
          0 -> ffiFree free ctx >> pure Nothing
          _ -> Just <$> Foreign.peek w
  lazyIOM iom

foreignListRef
  :: Storable a
  => FunPtr (Ptr () -> Ptr a -> IO Word8)
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

nextList :: (Show a, Storable a) => StablePtr (IORef [a]) -> Ptr a -> IO Bool
nextList list ptr = do
  ioref <- deRefStablePtr list
  readIORef ioref >>= \case
    []   -> pure False
    x:xs -> do
      writeIORef ioref xs
      Foreign.poke ptr x
      pure True

-- | nextList that catches exceptions.
--   If errPtr has changed then an exception has been thrown.
tryNextList :: Storable a => StablePtr (IORef [a]) -> Ptr a -> Ptr (StablePtr SomeException) -> IO Bool
tryNextList list ptr errPtr = do
  ioref <- deRefStablePtr list
  res <- try $ readIORef ioref >>= \case
    []   -> pure False
    x:xs -> do
      writeIORef ioref xs
      Foreign.poke ptr x
      pure True
  case res of
    Right a -> pure a
    Left err -> do
      newStablePtr err >>= poke errPtr
      pure False

type ForeignListType a
   = FunPtr (Ptr () -> Ptr a -> IO Word8)
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
foreignListRefChar  :: ForeignListType Char
foreignListRefChar  = foreignListRef

foreign export ccall foreignListRefU64 :: ForeignListType Word64
foreign export ccall foreignListRefU32 :: ForeignListType Word32
foreign export ccall foreignListRefU16 :: ForeignListType Word16
foreign export ccall foreignListRefU8  :: ForeignListType Word8
foreign export ccall foreignListRefI64 :: ForeignListType Int64
foreign export ccall foreignListRefI32 :: ForeignListType Int32
foreign export ccall foreignListRefI16 :: ForeignListType Int16
foreign export ccall foreignListRefI8  :: ForeignListType Int8
foreign export ccall foreignListRefChar  :: ForeignListType Char
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
nextListChar :: StablePtr (IORef [Char]) -> Ptr Char -> IO Bool
nextListChar = nextList
-- nextListBool :: StablePtr (IORef [Bool]) -> Ptr Bool -> IO Bool
-- nextListBool = nextList

tryNextList8 :: StablePtr (IORef [Word8]) -> Ptr Word8 -> Ptr (StablePtr SomeException) -> IO Bool
tryNextList8 = tryNextList
tryNextList16 :: StablePtr (IORef [Word16]) -> Ptr Word16 -> Ptr (StablePtr SomeException) -> IO Bool
tryNextList16 = tryNextList
tryNextList32 :: StablePtr (IORef [Word32]) -> Ptr Word32 -> Ptr (StablePtr SomeException) -> IO Bool
tryNextList32 = tryNextList
tryNextList64 :: StablePtr (IORef [Word64]) -> Ptr Word64 -> Ptr (StablePtr SomeException) -> IO Bool
tryNextList64 = tryNextList
tryNextListChar :: StablePtr (IORef [Char]) -> Ptr Char -> Ptr (StablePtr SomeException) -> IO Bool
tryNextListChar = tryNextList

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
foreign export ccall nextListChar :: StablePtr (IORef [Char]) -> Ptr Char -> IO Bool
foreign export ccall tryNextList8  :: StablePtr (IORef [Word8])  -> Ptr Word8  -> Ptr (StablePtr SomeException) -> IO Bool
foreign export ccall tryNextList16 :: StablePtr (IORef [Word16]) -> Ptr Word16 -> Ptr (StablePtr SomeException) -> IO Bool
foreign export ccall tryNextList32 :: StablePtr (IORef [Word32]) -> Ptr Word32 -> Ptr (StablePtr SomeException) -> IO Bool
foreign export ccall tryNextList64 :: StablePtr (IORef [Word64]) -> Ptr Word64 -> Ptr (StablePtr SomeException) -> IO Bool
foreign export ccall tryNextListChar :: StablePtr (IORef [Char]) -> Ptr Char -> Ptr (StablePtr SomeException) -> IO Bool

foreign export ccall loadloadload :: IO ()

deref_word64 :: StablePtr Word64 -> IO Word64
deref_word64 = deRefStablePtr

deref_int64 :: StablePtr Int64 -> IO Int64
deref_int64 = deRefStablePtr

foreign export ccall deref_word64 :: StablePtr Word64 -> IO Word64
foreign export ccall deref_int64 :: StablePtr Int64 -> IO Int64

show_exception :: StablePtr SomeException -> IO (StablePtr String)
show_exception ptr = do
  err <- deRefStablePtr ptr
  newStablePtr (displayException err)
foreign export ccall show_exception :: StablePtr SomeException -> IO (StablePtr String)

-- bytes

-- There's a few ways to copy bytes to/from haskell.
--
-- The obvious way is for the c-api land to expose a pointer and length. Either this pointer is only
-- provided for a small amount of time or it's done behind a foreign pointer (with a custom
-- destructor).
--
-- Another way is for ghc to allocate a ByteArray and give it to c-land to fill in. This can avoid
-- an unnessary copy, and the lifetime issues are handled in c land.

-- foreign export ccall newForeignPtr :: FinalizerPtr Word8 -> Ptr Word8 -> IO (ForeignPtr Word8)

newByteString :: FinalizerEnvPtr () Word8 -> Ptr () -> Ptr Word8 -> Int -> IO (StablePtr ByteString)
newByteString fin env ptr len = do
  fptr <- newForeignPtrEnv fin env ptr
  newStablePtr $! PS fptr 0 len

foreign export ccall newByteString :: FinalizerEnvPtr () Word8 -> Ptr () -> Ptr Word8 -> Int -> IO (StablePtr ByteString)

byteStringParts :: StablePtr ByteString -> Ptr (Ptr Word8) -> Ptr Int -> IO Bool
byteStringParts bsPtr ptrPtr lenPtr = do
  bs <- deRefStablePtr bsPtr
  let (fptr, offset, len) = toForeignPtr bs
  withForeignPtr fptr (\ptr -> poke ptrPtr (ptr `plusPtr` offset))
  poke lenPtr len
  pure True

printBS :: StablePtr ByteString -> IO ()
printBS bsPtr = deRefStablePtr bsPtr >>= print

foreign export ccall byteStringParts :: StablePtr ByteString -> Ptr (Ptr Word8) -> Ptr Int -> IO Bool
foreign export ccall printBS :: StablePtr ByteString -> IO ()

-- foreign import ccall "dynamic" ffiNextUnit ::
--   FunPtr (Ptr () -> Ptr () -> IO Word8)
--        -> Ptr () -> Ptr () -> IO Word8

-- ffiNext ::
--   FunPtr (Ptr () -> Ptr a -> IO Word8)
--        -> Ptr () -> Ptr a -> IO Word8
-- ffiNext fptr stb a = ffiNextUnit (castFunPtr fptr) stb (castPtr a)


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
  let dflags3 = dflags
        { DynFlags.hscTarget = DynFlags.HscInterpreted -- HscAsm
        , DynFlags.ghcMode   = DynFlags.CompManager
        , DynFlags.ghcLink   = DynFlags.LinkInMemory
        }
      platform = DynFlags.targetPlatform dflags3
      dflags3a = DynFlags.updateWays $ dflags3 { GHC.ways = DynFlags.interpWays }
      dflags3b = foldl DynFlags.gopt_set dflags3a
                $ concatMap (DynFlags.wayGeneralFlags platform)
                            DynFlags.interpWays
      dflags3c = foldl DynFlags.gopt_unset dflags3b
                $ concatMap (DynFlags.wayUnsetGeneralFlags platform)
                            DynFlags.interpWays
  _ <- GHC.setSessionDynFlags dflags3c
  pure ()

importModules :: Session -> [String] -> IO ()
importModules session modules = flip unGhc session $ do
  ghciHandle (\e -> liftIO (print e) >> pure ()) $
    GHC.setContext $ map (GHC.IIDecl . GHC.simpleImportDecl . mkModuleName) modules

compExpr :: Session -> String -> IO (Either GHCi.SerializableException GHC.HValue)
compExpr session expr = tryJust (Just . GHCi.toSerializableException) $ flip unGhc session $ do
  GHC.compileExpr expr

runDecl :: Session -> String -> IO (Either GHCi.SerializableException [Name])
runDecl session decl = tryJust (Just . GHCi.toSerializableException) $ flip unGhc session $ do
  names <- GHC.runDecls decl
  liftIO $ print (map (occNameString . occName) names)
  pure names

runExpr :: Session -> String -> IO GHC.ExecResult
runExpr session expr = flip unGhc session $
    -- let opts = GHC.execOptions
    --               { GHC.execSourceFile = "INTERACTIVE"
    --               , GHC.execLineNumber = 71273
    --               , GHC.execSingleStep = RunToCompletion
    --               , GHC.execWrap = \fhv -> EvalApp (EvalThis (evalWrapper st))
    --                                                (EvalThis fhv) }
    GHC.execStmt expr GHC.execOptions

compExprDyn :: Session -> String -> IO (Either GHCi.SerializableException Dynamic)
compExprDyn session expr = tryJust (Just . GHCi.toSerializableException) $ flip unGhc session $ do
  GHC.dynCompileExpr expr

compExprWithType :: Session -> TypeRep a -> String -> IO (Either GHCi.SerializableException a)
compExprWithType session ty expr = tryJust (Just . GHCi.toSerializableException) $ flip unGhc session $ do
  compileExprWithType ty expr

compileExprWithType :: TypeRep a -> String -> Ghc a
compileExprWithType ty expr = do
  -- For now use strings for the type but should probably point to real names by applying type
  -- signature to the parsed result. Is this still possible with a TypeRep? PrelNames has a bunch of
  -- names from prelude.
  parsed_expr <- GHC.parseExpr (expr <> " :: " <> show ty)
  hval <- GHC.compileParsedExpr parsed_expr
  return (unsafeCoerce# hval)

cleanup :: Session -> IO ()
cleanup session = flip unGhc session $ GHC.withCleanupSession (pure ())

rrr :: String -> IO ()
rrr expr = do
  session <- unsafeNewSession Nothing
  importModules session ["Prelude", "Plug2"]
  res <- compExpr session expr
  either print (\val -> print (unsafeCoerce val :: [Int])) res

  importModules session ["Prelude"]
  flip unGhc session $ loadModules [("/home/chris/Documents/hs-ffi-plugins/rs/examples/test.hs", Nothing)]
  putStrLn "Loaded the modules"
  _res <- runExpr session "myCoolerList"
  -- either print (\val -> print (unsafeCoerce val :: [Int])) res

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

char_type_rep :: IO (StablePtr SomeTypeRep)
char_type_rep = newStablePtr $ someTypeRep (Proxy @Char)

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
foreign export ccall char_type_rep :: IO (StablePtr SomeTypeRep)
foreign export ccall list_type_rep :: StablePtr SomeTypeRep -> IO (StablePtr SomeTypeRep)
foreign export ccall type_rep_fingerprint :: StablePtr SomeTypeRep -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign export ccall dyn_type_rep :: StablePtr Dynamic -> IO (StablePtr SomeTypeRep)
foreign export ccall dyn_val :: StablePtr Dynamic -> IO (StablePtr Any)

-- ghc api -------------------------------------------------------------

new_session :: CString -> IO (StablePtr Session)
new_session cstr = do
  lib >>= unsafeNewSession >>= newStablePtr
  where lib = if cstr == nullPtr then pure Nothing else Just <$> (peekCString cstr)

set_verbosity :: StablePtr Session -> Int -> IO ()
set_verbosity ptr verbosity = do
  session <- deRefStablePtr ptr
  flip unGhc session $ do
    dflags <- DynFlags.getDynFlags
    _ <- GHC.setSessionDynFlags dflags { GHC.verbosity = verbosity }
    pure ()

run_expr :: StablePtr Session -> CString -> IO ()
run_expr ptr cexpr = do
  session <- deRefStablePtr ptr
  expr <- peekCString cexpr
  compExpr session ("print (" <> expr <> ") :: IO ()") >>= \case
    Right io -> unsafeCoerce io
    Left (GHCi.EOtherException msg) -> putStrLn msg
    Left err -> print err

run_expr_dyn
  :: StablePtr Session
  -> CString
  -> Ptr (StablePtr Dynamic)
  -> IO Word64
run_expr_dyn ptr cexpr ptrPtr = do
  session <- deRefStablePtr ptr
  expr <- peekCString cexpr
  compExprDyn session expr >>= \case
    Right dyn -> do
      newStablePtr dyn >>= poke ptrPtr
      pure 0
    Left GHCi.EUserInterrupt -> pure 1
    Left (GHCi.EExitCode code) -> do
      newStablePtr (Dynamic typeRep code) >>= poke ptrPtr
      pure 2
    Left (GHCi.EOtherException msg) -> do
      newStablePtr (Dynamic typeRep msg) >>= poke ptrPtr
      pure 3

-- compExprWithType :: Session -> TypeRep a -> String -> IO (Either GHCi.SerializableException a)

run_expr_with_type
  :: StablePtr Session
  -> StablePtr SomeTypeRep
  -> CString
  -> Ptr (StablePtr Any)
  -> IO Word64
run_expr_with_type ptr ty cexpr ptrPtr = do
  session <- deRefStablePtr ptr
  SomeTypeRep ty <- deRefStablePtr ty
  Just HRefl <- pure $ eqTypeRep (typeRepKind ty) (typeRep @Type)
  expr <- peekCString cexpr
  compExprWithType session ty expr >>= \case
    Right dyn -> do
      newStablePtr (unsafeCoerce# dyn) >>= poke ptrPtr
      pure 0
    Left GHCi.EUserInterrupt -> pure 1
    Left (GHCi.EExitCode code) -> do
      newStablePtr (unsafeCoerce# $ Dynamic typeRep code) >>= poke ptrPtr
      pure 2
    Left (GHCi.EOtherException msg) -> do
      newStablePtr (unsafeCoerce# $ Dynamic typeRep msg) >>= poke ptrPtr
      pure 3

run_decl
  :: StablePtr Session
  -> CString
  -> Ptr (StablePtr Any)
  -> IO Word64
run_decl ptr cdecl ptrPtr = do
  session <- deRefStablePtr ptr
  decl <- peekCString cdecl
  runDecl session decl >>= \case
    Right names -> do
      newStablePtr (unsafeCoerce# names) >>= poke ptrPtr
      pure 0
    Left GHCi.EUserInterrupt -> pure 1
    Left (GHCi.EExitCode code) -> do
      newStablePtr (unsafeCoerce# $ Dynamic typeRep code) >>= poke ptrPtr
      pure 2
    Left (GHCi.EOtherException msg) -> do
      newStablePtr (unsafeCoerce# $ Dynamic typeRep msg) >>= poke ptrPtr
      pure 3
foreign export ccall run_decl :: StablePtr Session -> CString -> Ptr (StablePtr Any) -> IO Word64


-- | The import paths are usually done from -i arguments on the cmdline.
set_import_paths :: StablePtr Session -> Int -> Ptr CString -> IO ()
set_import_paths ptr n importsPtr = do
  session <- deRefStablePtr ptr
  imports <- mapM (\n -> peekElemOff importsPtr n >>= peekCString) [0..n-1]
  flip unGhc session $ do
    dflags <- DynFlags.getDynFlags
    _ <- GHC.setSessionDynFlags dflags { GHC.importPaths = imports }
    pure ()

load_modules :: StablePtr Session -> Int -> Ptr CString -> IO Word64
load_modules ptr n modulesPtr = do
  session <- deRefStablePtr ptr
  modules <- mapM (\n -> peekElemOff modulesPtr n >>= peekCString) [0..n-1]
  flip unGhc session $ do
    success <- ghciHandle (\e -> liftIO (print e) >> pure 0) $ loadModules (map (,Nothing) modules)
    pure success

doLoad :: GHC.GhcMonad m => Bool -> GHC.LoadHowMuch -> m GHC.SuccessFlag
doLoad retain_context howmuch = do
  -- Enable buffering stdout and stderr as we're compiling. Keeping these
  -- handles unbuffered will just slow the compilation down, especially when
  -- compiling in parallel.
  GHC.gbracket (liftIO $ do
                   IO.hSetBuffering IO.stdout IO.LineBuffering
                   IO.hSetBuffering IO.stderr IO.LineBuffering)
           (\_ ->
            liftIO $ do IO.hSetBuffering IO.stdout IO.NoBuffering
                        IO.hSetBuffering IO.stderr IO.NoBuffering) $ \_ -> do
      ok <- trySuccess $ GHC.load howmuch
      -- afterLoad ok retain_context
      return ok

trySuccess :: GHC.GhcMonad m => m GHC.SuccessFlag -> m GHC.SuccessFlag
trySuccess act =
    handleSourceError (\e -> do GHC.printException e
                                return GHC.Failed) $ do
      act

ghciHandle :: (HasDynFlags m, ExceptionMonad m) => (SomeException -> m a) -> m a -> m a
ghciHandle h m = gmask $ \restore -> do
  -- Force dflags to avoid leaking the associated HscEnv
  dflags <- DynFlags.getDynFlags
  dflags `seq` GHC.gcatch (restore (GHC.prettyPrintGhcErrors dflags m)) $ \e -> restore (h e)

-- | taken from GHCi.UI
loadModules :: [(FilePath, Maybe GHC.Phase)] -> Ghc Word64
loadModules files = do
  targets <- mapM (uncurry GHC.guessTarget) files
  hsc_env <- GHC.getSession
  _ <- GHC.abandonAll
  GHC.setTargets []
  _ <- GHC.load GHC.LoadAllTargets
  GHC.setTargets targets
  -- success <- GHC.load GHC.LoadAllTargets
  success <- doLoad False GHC.LoadAllTargets -- is GHC.load good enough?

  loaded_mods <- getLoadedModules
  -- -- modulesLoadedMsg ok loaded_mods
  let retain_context = False
  liftIO $ putStrLn $ "There are " <> show (length loaded_mods) <> " loaded modules"
  liftIO $ print $ map (moduleNameString . ms_mod_name) loaded_mods
  setContextAfterLoad retain_context loaded_mods
  pure $ case success of GHC.Succeeded -> 1; GHC.Failed -> 0

setContextAfterLoad :: Bool -> [GHC.ModSummary] -> Ghc ()
setContextAfterLoad keep_ctxt [] = do
  -- setContextKeepingPackageModules keep_ctxt []
  pure ()
setContextAfterLoad keep_ctxt ms = do
  -- load a target if one is available, otherwise load the topmost module.
  targets <- GHC.getTargets
  case [ m | Just m <- map (findTarget ms) targets ] of
        []    ->
          let graph = GHC.mkModuleGraph ms
              graph' = flattenSCCs (GHC.topSortModuleGraph True graph Nothing)
          in load_this (last graph')
        (m:_) ->
          load_this m
 where
   findTarget mds t
    = case filter (`matches` t) mds of
        []    -> Nothing
        (m:_) -> Just m

   summary `matches` Target (TargetModule m) _ _
        = GHC.ms_mod_name summary == m
   summary `matches` Target (TargetFile f _) _ _
        | Just f' <- GHC.ml_hs_file (GHC.ms_location summary)   = f == f'
   _ `matches` _
        = False

   load_this summary | m <- GHC.ms_mod summary = do
        is_interp <- GHC.moduleIsInterpreted m
        dflags <- DynFlags.getDynFlags
        let star_ok = is_interp && not (DynFlags.safeLanguageOn dflags)
              -- We import the module with a * iff
              --   - it is interpreted, and
              --   - -XSafe is off (it doesn't allow *-imports)
        let new_ctx | star_ok   = [mkIIModule (GHC.moduleName m)]
                    | otherwise = [mkIIDecl   (GHC.moduleName m)]
        -- setContextKeepingPackageModules keep_ctxt new_ctx
        GHC.setContext new_ctx

-- -- | Keep any package modules (except Prelude) when changing the context.
-- setContextKeepingPackageModules
--         :: Bool                 -- True  <=> keep all of remembered_ctx
--                                 -- False <=> just keep package imports
--         -> [InteractiveImport]  -- new context
--         -> Ghc ()

-- setContextKeepingPackageModules keep_ctx trans_ctx = do

--   st <- getGHCiState
--   let rem_ctx = remembered_ctx st
--   new_rem_ctx <- if keep_ctx then return rem_ctx
--                              else keepPackageImports rem_ctx
  -- setGHCiState st{ remembered_ctx = new_rem_ctx,
  --                  transient_ctx  = filterSubsumed new_rem_ctx trans_ctx }
  -- setGHCContextFromGHCiState

mkIIModule :: ModuleName -> InteractiveImport
mkIIModule = IIModule

mkIIDecl :: ModuleName -> InteractiveImport
mkIIDecl = IIDecl . GHC.simpleImportDecl

getLoadedModules :: GHC.GhcMonad m => m [GHC.ModSummary]
getLoadedModules = do
  graph <- GHC.getModuleGraph
  filterM (GHC.isLoaded . GHC.ms_mod_name) (GHC.mgModSummaries graph)

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
foreign export ccall set_verbosity :: StablePtr Session -> Int -> IO ()
foreign export ccall run_expr :: StablePtr Session -> CString -> IO ()
foreign export ccall run_expr_dyn :: StablePtr Session -> CString -> Ptr (StablePtr Dynamic) -> IO Word64
foreign export ccall cleanup_session :: StablePtr Session -> IO ()
foreign export ccall import_modules :: StablePtr Session -> Int -> Ptr CString -> IO ()
foreign export ccall debugging :: StablePtr Session -> IO ()
foreign export ccall load_modules :: StablePtr Session -> Int -> Ptr CString -> IO Word64
foreign export ccall set_import_paths :: StablePtr Session -> Int -> Ptr CString -> IO ()
foreign export ccall run_expr_with_type :: StablePtr Session -> StablePtr SomeTypeRep -> CString -> Ptr (StablePtr Any) -> IO Word64

------------------------------------------------------------------------
-- iface files
------------------------------------------------------------------------

readBinIface' :: FilePath -> IO ModIface
readBinIface' hi_path = do
  mySettings <- initSysTools (Just libdir) -- how should we really set the top dir?
  llvmConfig <- initLlvmConfig (Just libdir)
  dflags <- initDynFlags (defaultDynFlags mySettings llvmConfig)
  e <- newHscEnv dflags
  initTcRnIf 'r' e undefined undefined (readBinIface IgnoreHiWay QuietBinIFaceReading hi_path)

pp :: Outputable a => a -> IO ()
pp a = GHC.runGhc (Just libdir) $ do
  dflags <- DynFlags.getDynFlags
  liftIO $ putStrLn (showPpr dflags a)

declInfo :: IfaceDecl -> IO ()
declInfo = \case
  IfaceId name ty deets inf -> do
    putStrLn $ "IfaceId"
    putStrLn $ "name: " <> getOccString name
    putStr "ty: " >> pp ty
    -- putStrLn $ showIfaceTy ty
    case deets of
      IfVanillaId                     -> putStrLn "IfVanillaId"
      IfRecSelId _eitherConDecl _bool -> putStrLn "IfRecSelId"
      IfDFunId                        -> putStrLn "IfDFunId"
    case inf of
      NoInfo       -> putStrLn "NoInfo"
      HasInfo info -> putStrLn $ "HasInfo " <> show (length info)
    putStrLn ""
  IfaceData name bndrs resKind _cty _roles ctx conDecls isGADT parent -> do
    putStrLn $ "IfaceData"
    putStrLn $ "name: " <> getOccString name
    putStr "ty: " >> pp resKind
    putStr "binders: " >> pp bndrs
    putStr "ctx: " >> pp ctx
    putStr "conDecls: " >> case conDecls of
      IfAbstractTyCon -> putStrLn "IfAbstractTyCon"
      IfDataTyCon cons -> putStrLn "IfDataTyCon" >> (forM_ cons $ \con -> do
        putStr "  name: " >> pp (ifConName con)
        putStr "  ifConExTvs: " >> pp (ifConExTvs con)
        putStr "  ifConUserTvBinders: " >> pp (ifConUserTvBinders con)
        putStr "  ifConEqSpec: " >> pp (ifConEqSpec con)
        putStr "  ifConCtxt: " >> pp (ifConCtxt con)
        putStr "  ifConArgTys: " >> pp (ifConArgTys con)
        putStr "  ifConFields: " >> pp (ifConFields con))
      IfNewTyCon con -> do
        putStrLn "IfAbstractTyCon"
        putStr "  name: " >> pp (ifConName con)
        putStr "  ifConExTvs: " >> pp (ifConExTvs con)
        putStr "  ifConUserTvBinders: " >> pp (ifConUserTvBinders con)
        putStr "  ifConEqSpec: " >> pp (ifConEqSpec con)
        putStr "  ifConCtxt: " >> pp (ifConCtxt con)
        putStr "  ifConArgTys: " >> pp (ifConArgTys con)
        putStr "  ifConFields: " >> pp (ifConFields con)
    putStr "parent: " >> pp parent
    putStr "isGADT: " >> pp isGADT
    putStr "ifaceCons: " >> pp parent
    putStr ""
  _ -> putStrLn "unknown decl\n"

read_bin_iface :: CString -> IO (StablePtr ModIface)
read_bin_iface cstr = peekCString cstr >>= readBinIface' >>= newStablePtr

pp_iface :: StablePtr ModIface -> IO ()
pp_iface ptr = do
  iface <- deRefStablePtr ptr
  forM_ (mi_decls iface) $ \(_, decl) -> do
    pp decl
    declInfo decl

foreign export ccall read_bin_iface :: CString -> IO (StablePtr ModIface)
foreign export ccall pp_iface :: StablePtr ModIface -> IO ()

-- new_session cstr = do
--   lib >>= unsafeNewSession >>= newStablePtr
--   where lib = if cstr == nullPtr then pure Nothing else Just <$> (peekCString cstr)
