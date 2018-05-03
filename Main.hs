module Main (main) where
import Data.List
import System.Environment
import Bio.Fasta

main = do
  args <- getArgs
  let filename = args!!0
  fastas <- read_fasta filename
  putStr $ concatMap show fastas
