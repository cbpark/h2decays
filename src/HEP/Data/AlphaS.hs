module HEP.Data.AlphaS (AlphaS, initAlphaS, alphasQ) where

import HEP.Data.Constants     (alphasMZ, mZ, mt)
import HEP.Data.Kinematics    (Mass (..))

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
mkAlphaS mt0 mz0 alpha0 = liftIO $ do
    CAlphaS cas <- c_mkAlphaS (realToFrac mt0) (realToFrac mz0) (realToFrac alpha0)
    as <- newForeignPtr finalizerFree cas
    return (AlphaS as)

foreign import ccall "alphas.h alphasQ" c_alphasQ
    :: CAlphaS -> CDouble -> IO CDouble

alphasQ :: MonadIO m => AlphaS -> Double -> m Double
alphasQ (AlphaS as) q = liftIO $
    withForeignPtr as (\a -> realToFrac <$> c_alphasQ (CAlphaS a) (realToFrac q))

initAlphaS :: MonadIO m => m AlphaS
initAlphaS = mkAlphaS (getMass mt) (getMass mZ) alphasMZ
