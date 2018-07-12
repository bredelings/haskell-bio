module Main (main) where
import Data.List
import System.Environment
import Bio.Fasta


import Numeric.LinearAlgebra as LA

jc69_line n i = (replicate i (fromIntegral 1::Double))
                ++[fromIntegral (-(n-1))::Double]
                ++(replicate (n-1-i) (fromIntegral 1::Double))

jc69 n = (n><n) (concat [jc69_line n i | i <- [0..n-1]]) :: Matrix R

main = do
  args <- getArgs
  let filename = args!!0
  fastas <- read_fasta filename
  putStr $ concatMap show fastas
  putStr $ show $ jc69 4
