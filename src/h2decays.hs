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

    let mHVal    = mH input
        mAVal    = fromMaybe mHVal (mA input)
        mHpVal   = mHp input
        tanbVal  = tanb input
        cosbaVal = cosba input

    as <- initAlphaS
    let inp = [ InputParam { _mdtyp = mdtypVal
                           , _mH    = Mass mHVal
                           , _mA    = Mass mAVal
                           , _mHp   = Mass mHpVal
                           , _angs  = mkAngles tanbVal cosbaVal }
              , InputParam { _mdtyp = mdtypVal
                           , _mH    = Mass (mHVal + 0.05)
                           , _mA    = Mass (mAVal + 0.05)
                           , _mHp   = Mass mHpVal
                           , _angs  = mkAngles tanbVal cosbaVal }]

    brs <- mapM (getBRH2 as) inp

    let outfile = fromMaybe "output_h2.dat" (output input)
    withBinaryFile outfile WriteMode $ \h -> do
        B.hPutStrLn h header
        mapM_ (hPutBuilder h) brs

    putStrLn $ "-- " ++ outfile ++ " generated."

data InputArgs w = InputArgs
    { mtype  :: w ::: Maybe Int    <?> "model type (either 1 or 2)"
    , mH     :: w ::: Double       <?> "heavy Higgs mass"
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
          , "hh", "HpHm", "HW", "HWstar"
          ])
