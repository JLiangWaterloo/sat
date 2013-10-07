module Main where

import Control.Applicative
import Data.List
import Numeric

main :: IO()
main = putStr =<< showDouble . average . map read . words <$> getContents

showDouble :: Double -> String
showDouble d = showFFloat Nothing d ""

average :: [Double] -> Double
average xs = sum xs / genericLength xs