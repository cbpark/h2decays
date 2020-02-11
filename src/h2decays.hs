{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           HEP.Data.AlphaS         (initAlphaS)
import           HEP.Data.Kinematics
import           HEP.Data.THDM
import           HEP.Data.Util           (mkAngles)

import           Data.ByteString.Builder (hPutBuilder)
import           Data.ByteString.Char8   (ByteString)
import qualified Data.ByteString.Char8   as B
import qualified Data.Vector             as V
import           Options.Generic

import           Control.Monad           (when)
import           Data.Maybe              (fromMaybe)
import           System.Exit             (die)
import           System.IO               (IOMode (..), withBinaryFile)

main :: IO ()
main = do
    input <- unwrapRecord "Calculate the branching ratio of heavy Higgs boson"

    let mdtyp = fromMaybe 2 (mtype input)
        mdtypVal | mdtyp == 1 = TypeI
                 | mdtyp == 2 = TypeII
                 | otherwise  = UnknownType
    when (mdtypVal == UnknownType) $ die "The type must be either 1 or 2."

    let (mHVal1, mHVal2) = (,) <$> minimum <*> maximum $ mH input
        npoints = floor $ (mHVal2 - mHVal1) / stepsize + 1
        mHVals = V.generate npoints (\i -> mHVal1 + fromIntegral i * stepsize)
        mAVals = fromMaybe mHVals (V.replicateM npoints (mA input))
        mHpVal   = mHp input
        tanbVal  = tanb input
        cosbaVal = cosba input

    putStrLn $ "-- m_{H+} = " ++ show mHpVal ++ ", tan(beta) = " ++ show tanbVal
        ++ ", cos(beta - alpha) = " ++ show cosbaVal

    as <- initAlphaS
    let inps = V.zipWith (\mHVal mAVal -> InputParam
                                          { _mdtyp  = mdtypVal
                                          , _mH     = Mass mHVal
                                          , _mA     = Mass mAVal
                                          , _mHp    = Mass mHpVal
                                          , _angs   = mkAngles tanbVal cosbaVal
                                          }) mHVals mAVals
    putStrLn "-- Calculating the branching ratios of the heavy Higgs boson..."
    brs <- V.mapM (getBRH2 as) inps
    putStrLn "-- ... done."

    let outfile = fromMaybe "output_h2.dat" (output input)
    withBinaryFile outfile WriteMode $ \h -> do
        B.hPutStrLn h header
        mapM_ (hPutBuilder h) brs

    putStrLn $ "-- " ++ outfile ++ " generated."

data InputArgs w = InputArgs
    { mtype  :: w ::: Maybe Int    <?> "model type (either 1 or 2)"
    , mH     :: w ::: [Double]     <?> "heavy Higgs mass"
    , mA     :: w ::: Maybe Double <?> "heavy mass scale (m_A if MSSM)"
    , mHp    :: w ::: Double       <?> "charged Higgs mass"
    , tanb   :: w ::: Double       <?> "tan(beta)"
    , cosba  :: w ::: Double       <?> "cos(beta-alpha)"
    , output :: w ::: Maybe String <?> "the name of the output file"
    } deriving Generic

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)

header :: ByteString
header = B.pack $ "# " <>
         foldl1 (\v1 v2 -> v1 <> ", " <> v2)
         (zipWith (\n v -> "(" <> show n <> ") " <> v) ([1..] :: [Int])
          [ "type", "mH", "mA", "mHp", "tanb", "cosba"
          , "width"
          , "tt", "bb", "cc", "tautau", "mumu"
          , "ww", "zz", "gammagamma", "gg"
          , "hh", "HpHm", "HpmTB"
          , "HpmW"
          ])

stepsize :: Double
stepsize = 0.1
