module HEP.Data.AlphaS (AlphaS, mkAlphaS, alphasQ) where

import Control.Monad.IO.Class (MonadIO (..))
import Foreign.C.Types        (CDouble (..))
import Foreign.ForeignPtr     (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc  (finalizerFree)
import Foreign.Ptr            (Ptr)

newtype CAlphaS = CAlphaS (Ptr CAlphaS)
newtype AlphaS = AlphaS (ForeignPtr CAlphaS)

foreign import ccall "alphs.h mkAlphaS" c_mkAlphaS
    :: CDouble -> CDouble -> CDouble -> IO CAlphaS

mkAlphaS :: MonadIO m => Double -> Double -> Double -> m AlphaS
mkAlphaS mt mz alpha = liftIO $ do
    CAlphaS cas <- c_mkAlphaS (realToFrac mt) (realToFrac mz) (realToFrac alpha)
    as <- newForeignPtr finalizerFree cas
    return (AlphaS as)

foreign import ccall "alphas.h alphasQ" c_alphasQ
    :: CAlphaS -> CDouble -> IO CDouble

alphasQ :: MonadIO m => AlphaS -> Double -> m Double
alphasQ (AlphaS as) q = liftIO $
    withForeignPtr as (\a -> realToFrac <$> c_alphasQ (CAlphaS a) (realToFrac q))
