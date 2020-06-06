{-# LANGUAGE MagicHash,UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell          #-}
module System.Plugins.Export
  -- callback ,
-- unsafeSizeof
 where


import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable
import Data.Word
import Control.Concurrent
import GHC.Exts
import Foreign

import Data.IORef
import System.IO.Unsafe

import Foreign.StablePtr
import Foreign.C (CString, peekCString)

import qualified System.Plugins.Load as Load

-- foreign import ccall "dynamic" ioNext :: FunPtr (Ptr () -> IO Word64) -> Ptr () -> IO Word64
-- foreign import ccall "dynamic" iofd :: FunPtr (Ptr () -> IO Bool) -> Ptr () -> IO Bool

foreign import ccall "dynamic" ffiNext ::
  FunPtr (Ptr () -> Ptr Word64 -> IO Bool)
       -> Ptr () -> Ptr Word64 -> IO Bool

foreign import ccall "dynamic" ffiFree ::
  FunPtr (Ptr () -> IO ()) -> Ptr () -> IO ()

stableRef :: a -> IO (StablePtr (IORef a))
stableRef a = newIORef a >>= newStablePtr

loadList
  :: CString -- ^ object file
  -> CString -- ^ symbol name
  -> Ptr (StablePtr (IORef [Word64]))
  -- ^ pointer that list will be written to
  -> IO Bool
loadList objC symC ptrPtr = do
  putStrLn "loadList has been called!"
  obj <- peekCString objC
  sym <- peekCString symC
  putStrLn $ "the arguemtns are " <> obj <> " " <> sym
  loaded <- Load.load obj [] [] sym
  case loaded of
    Load.LoadFailure e -> do
      putStrLn "LOAD FAILURE"
      print e
      pure False
    Load.LoadSuccess _m a -> do
      putStrLn "LOAD SUCCESS"
      ioRef <- newIORef (a :: [Word64])
      stbPtr <- newStablePtr ioRef
      Foreign.poke ptrPtr stbPtr
      pure True

foreignList
  :: FunPtr (Ptr () -> Ptr Word64 -> IO Bool)
  -> FunPtr (Ptr () -> IO ())
  -> Ptr ()
  -> IO [Word64]
foreignList next free ctx = do
  let iom = alloca $ \w -> do
        -- print w
        ffiNext next ctx w >>= \case
          True -> Just <$> Foreign.peek w
          False -> ffiFree free ctx >> pure Nothing
  lazyIOM iom

foreignListRef
  :: FunPtr (Ptr () -> Ptr Word64 -> IO Bool)
  -> FunPtr (Ptr () -> IO ())
  -> Ptr ()
  -> IO (StablePtr (IORef [Word64]))
foreignListRef next free ctx = do
  foreignList next free ctx >>= stableRef

-- | Clone a stable pointer to an ioref list.
cloneStableRef
  :: StablePtr (IORef [Word64])
  -> IO (StablePtr (IORef [Word64]))
cloneStableRef ptr = do
  ioref <- deRefStablePtr ptr
  xs <- readIORef ioref
  ioref' <- newIORef xs
  newStablePtr ioref'

-- | Clone a stable pointer to an ioref list.
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

nextList :: Storable a => StablePtr (IORef [a]) -> Ptr a -> IO Bool
nextList list ptr = do
  ioref <- deRefStablePtr list
  readIORef ioref >>= \case
    []   -> pure False
    x:xs -> do
      writeIORef ioref xs
      Foreign.poke ptr x
      pure True

foreign export ccall foreignListRef
  :: FunPtr (Ptr () -> Ptr Word64 -> IO Bool)
  -> FunPtr (Ptr () -> IO ())
  -> Ptr ()
  -> IO (StablePtr (IORef [Word64]))

nextList8 :: StablePtr (IORef [Word8]) -> Ptr Word8 -> IO Bool
nextList8 = nextList
nextList16 :: StablePtr (IORef [Word16]) -> Ptr Word16 -> IO Bool
nextList16 = nextList
nextList32 :: StablePtr (IORef [Word32]) -> Ptr Word32 -> IO Bool
nextList32 = nextList
nextList64 :: StablePtr (IORef [Word64]) -> Ptr Word64 -> IO Bool
nextList64 = nextList
nextListBool :: StablePtr (IORef [Bool]) -> Ptr Bool -> IO Bool
nextListBool = nextList

loadloadload :: IO ()
loadloadload = pure ()

foreign export ccall hello :: IO ()
foreign export ccall mkNewList :: IO (StablePtr (IORef [Word64]))
foreign export ccall cloneStableRef
  :: StablePtr (IORef [Word64])
  -> IO (StablePtr (IORef [Word64]))

foreign export ccall freeStable :: StablePtr () -> IO ()

foreign export ccall nextListBool :: StablePtr (IORef [Bool]) -> Ptr Bool -> IO Bool
foreign export ccall nextList8 :: StablePtr (IORef [Word8]) -> Ptr Word8 -> IO Bool
foreign export ccall nextList16 :: StablePtr (IORef [Word16]) -> Ptr Word16 -> IO Bool
foreign export ccall nextList32 :: StablePtr (IORef [Word32]) -> Ptr Word32 -> IO Bool
foreign export ccall nextList64 :: StablePtr (IORef [Word64]) -> Ptr Word64 -> IO Bool

foreign export ccall loadloadload :: IO ()
foreign export ccall loadList
  :: CString
  -> CString
  -> Ptr (StablePtr (IORef [Word64]))
  -> IO Bool
