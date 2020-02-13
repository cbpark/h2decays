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
import           System.IO             (IOMode (..), withFile)

main :: IO ()
main = do
    input <- unwrapRecord "Calculate the branching ratio of charged Higgs boson"

    let mdtyp = fromMaybe 2 (mtype input)
        mdtypVal | mdtyp == 1 = TypeI
                 | mdtyp == 2 = TypeII
                 | otherwise  = UnknownType
    when (mdtypVal == UnknownType) $ die "The type must be either 1 or 2."

    let step = fromMaybe 0.5 (stepsize input)
        (mHpVals, _) = mkPoints step (mHp input)
        (tanbVal, cosbaVal) = (,) <$> tanb <*> cosba $ input

    putStrLn $ "-- tan(beta) = " ++ show tanbVal
        ++ ", cos(beta - alpha) = " ++ show cosbaVal

    as <- initAlphaS
    let inps = V.map (\mHpVal -> InputParam
                                 { _mdtyp = mdtypVal
                                 , _mH    = Mass 0
                                 , _mA    = Mass 0
                                 , _mHp   = Mass mHpVal
                                 , _angs  = mkAngles tanbVal cosbaVal
                                 }) mHpVals

    putStrLn "-- Calculating the branching ratios of the charged Higgs boson..."

    let outfile = fromMaybe "output_hp.dat" (output input)
    withFile outfile WriteMode $ \h -> do
        hPutStrLn h header
        runEffect $ each inps >-> getBRHp as >-> printBR h

    putStrLn $ "-- " ++ outfile ++ " generated."

data InputArgs w = InputArgs
    { mtype    :: w ::: Maybe Int    <?> "model type (either 1 or 2)"
    , mHp      :: w ::: [Double]     <?> "charged Higgs mass"
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
          [ "type", "mHp", "tanb", "cosba", "width"
          , "tb", "cs", "taunu", "munu", "wh"
          ])
