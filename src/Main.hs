module Main where

import HEP.Data.AlphaS (alphasQ, mkAlphaS)

main :: IO ()
main = do
    as <- mkAlphaS 173.0 91.188 0.118
    alphasQ as 100 >>= print
