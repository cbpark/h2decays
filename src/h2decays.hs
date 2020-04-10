{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           HEP.Data.AlphaS       (initAlphaS)
import           HEP.Data.Kinematics   (Mass (..))
import           HEP.Data.THDM
import           HEP.Data.Util         (mkAngles, mkPoints)

import           Data.ByteString.Char8 (ByteString, hPutStrLn, pack)
import qualified Data.Vector           as V
import           Options.Generic
import           Pipes                 (each, runEffect, (>->))

import           Control.Monad         (when)
import           Data.Maybe            (fromMaybe)
import           System.Exit           (die)
import           System.IO             (IOMode (..), stdout, withFile)

main :: IO ()
main = do
    input <- unwrapRecord "Calculate the branching ratio of heavy Higgs boson"

    let mdtyp = fromMaybe 2 (mtype input)
        mdtypVal | mdtyp == 1 = TypeI
                 | mdtyp == 2 = TypeII
                 | otherwise  = UnknownType
    when (mdtypVal == UnknownType) $ die "-- The type must be either 1 or 2."

    let step = fromMaybe 0.5 (stepsize input)
        (tanbVal, cosbaVal) = (,) <$> tanb <*> cosba $ input
        (mHVals, npoints) = mkPoints step (mH input)
        mHpVal = mHp input
        mAVal  = fromMaybe mHpVal (mA input)
        m12Vals = fromMaybe (defaultM12 tanbVal <$> mHVals)
                  (V.replicateM npoints (m12 input))

    putStrLn $ "-- m_{H+} = " ++ show mHpVal ++ ", tan(beta) = " ++ show tanbVal
        ++ ", cos(beta - alpha) = " ++ show cosbaVal

    as <- initAlphaS
    let inps = V.zipWith (\mHVal m12Val -> InputParam
                                           { _mdtyp = mdtypVal
                                           , _mH    = Mass mHVal
                                           , _mA    = Mass mAVal
                                           , _mHp   = Mass mHpVal
                                           , _m12   = Mass m12Val
                                           , _angs  = mkAngles tanbVal cosbaVal
                                           }) mHVals m12Vals

    putStrLn "-- Calculating the branching ratios of the heavy Higgs boson..."

    let writeOutput h = runEffect $ each inps >-> getBRH2 as >-> printBR h
    case output input of
        Nothing      -> writeOutput stdout
        Just outfile -> do withFile outfile WriteMode $ \h -> do
                               hPutStrLn h header
                               writeOutput h
                           putStrLn $ "-- " ++ outfile ++ " generated."

data InputArgs w = InputArgs
    { mtype    :: w ::: Maybe Int    <?> "model type (either 1 or 2)"
    , mH       :: w ::: [Double]     <?> "heavy Higgs mass"
    , mA       :: w ::: Maybe Double <?> "CP-odd Higgs mass"
    , mHp      :: w ::: Double       <?> "charged Higgs mass"
    , m12      :: w ::: Maybe Double <?> "soft Z2 breaking term"
    , tanb     :: w ::: Double       <?> "tan(beta)"
    , cosba    :: w ::: Double       <?> "cos(beta-alpha)"
    , stepsize :: w ::: Maybe Double <?> "step size (default: 0.5)"
    , output   :: w ::: Maybe String <?> "the name of the output file"
    } deriving Generic

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)

header :: ByteString
header = pack $ "# " <>
         foldl1 (\v1 v2 -> v1 <> ", " <> v2)
         (zipWith (\n v -> "(" <> show n <> ") " <> v) ([1..] :: [Int])
          [ "type", "mH", "mA", "mHp", "m12", "tanb", "cosba"
          , "width"
          , "tt", "bb", "cc", "tautau", "mumu"
          , "ww", "zz", "gammagamma", "gg"
          , "hh", "HpHm", "HpmW", "AZ"
          ])
