{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Sat
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
         ["variable"] -> putStr =<< printGraph . graphVariable . parseDimacs <$> getContents
         ["clause"]   -> putStr =<< printGraph . graphClause . parseDimacs <$> getContents
         ["literal"]  -> putStr =<< printGraph . graphLiteral . parseDimacs <$> getContents
         _            -> error "Unknown argument, must be either \"variable\" or \"clause\" or \"literal\"."
         

graphVariable :: Sat -> [(Int, Int)]
graphVariable s =
    s >>= mapClause
    where
        mapClause :: Clause -> [(Int, Int)]
        mapClause = pairs . map abs
        
graphLiteral :: Sat -> [(Int, Int)]
graphLiteral s =
    map (mapTuple mapLiteral) $ s >>= mapClause
    where
        mapClause :: Clause -> [(Int, Int)]
        mapClause c = pairs c ++ excludedMiddles c
        mapTuple f (a, b) = (f a, f b)
        mapLiteral l
            | l < 0     = (-l * 2)
            | otherwise = l * 2 -1
        excludedMiddles c = [(x, -x) | x <- c]

graphClause :: Sat -> [(Int, Int)]
graphClause s =
    map canonicalPair (map nub collisions >>= pairs)
    where
        collisions :: [[Int]]
        collisions = Map.elems $ foldr mapClause Map.empty $ zip [1..] $ map nub s
        
        mapClause :: (Int, Clause) -> Map Int [Int] -> Map Int [Int]
        mapClause (i, c) m = foldr (mapLit [i]) m $ map abs c
        
        mapLit   :: Ord a => [a1] -> a -> Map a [a1] -> Map a [a1]
        mapLit = flip $ Map.insertWith (++)
        
printGraph :: [(Int, Int)] -> String
printGraph g = unlines [show a ++ " " ++ show b | (a, b) <- g]
        
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (x ,) xs ++ pairs xs

canonicalPair :: Ord a => (a, a) -> (a, a)
canonicalPair (a, b)
    | a <= b    = (a, b)
    | otherwise = (b, a)