{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, NamedFieldPuns, LambdaCase, DeriveDataTypeable #-}

module Png
  ( Png(), PngRead
  , PngException
  , createRead
  , destroyRead
  , fatal
  ) where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Typeable
import Foreign
import Foreign.C

data Opaque

newtype PngPtr = PngPtr (Ptr Opaque)
newtype InfoPtr = InfoPtr (Ptr Opaque)

data PngRead
data Png mode = Png
  { pngPtr :: PngPtr
  , errorRef :: IORef (Maybe String)
  , errorPointer :: FunPtr ErrorCallback
  }

ptr :: Png a -> Ptr Opaque
ptr png = let PngPtr p = pngPtr png in p

data PngException = PngException String
  deriving ( Show, Typeable )

instance Exception PngException

type ErrorCallback = PngPtr -> CString -> IO ()

foreign import ccall "libpng-safe.h safe_create_read_struct"
  safe_create_read_struct :: FunPtr ErrorCallback -> FunPtr ErrorCallback -> IO (Ptr Opaque)

foreign import ccall "wrapper"
  mkErrorCallback :: ErrorCallback -> IO (FunPtr ErrorCallback)

errorHandler :: IORef (Maybe String) -> PngPtr -> CString -> IO ()
errorHandler errorRef _ cmsg
  = peekCString cmsg >>= writeIORef errorRef . Just

createRead :: IO (Png PngRead)
createRead = do
  errorRef <- newIORef Nothing
  errorPointer <- mkErrorCallback (errorHandler errorRef)
  pngPtr <- safe_create_read_struct errorPointer nullFunPtr
  unless (pngPtr /= nullPtr) $ throw (PngException "Could not create read struct.")
  return $ Png (PngPtr pngPtr) errorRef errorPointer

check :: Png m -> IO ()
check Png { errorRef }
  = readIORef errorRef >>= \case
    Nothing -> return ()
    Just err -> throw (PngException err)

foreign import ccall "png.h png_destroy_read_struct"
  png_destroy_read_struct :: Ptr (Ptr Opaque) -> Ptr (Ptr Opaque) -> Ptr (Ptr Opaque) -> IO ()

destroyRead :: Png PngRead -> IO ()
destroyRead Png { pngPtr = PngPtr pngPtr_, errorPointer } = do
  freeHaskellFunPtr errorPointer
  with pngPtr_ $ \pngPtrPtr -> png_destroy_read_struct pngPtrPtr nullPtr nullPtr

foreign import ccall safe "libpng-safe.h SHIM_png_error"
  png_error :: Ptr Opaque -> CString -> IO ()

fatal :: Png a -> IO ()
fatal png = withCString "boom!" (png_error (ptr png)) >> check png