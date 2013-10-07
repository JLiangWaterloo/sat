module Main where

import Control.Applicative
import Numeric

main :: IO()
main = putStr =<< showDouble . minimum . map read . words <$> getContents

showDouble :: Double -> String
showDouble d = showFFloat Nothing d ""