{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Sat
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
         ["variable"] -> putStr =<< printGraph . nubSet . graphVariable . parseDimacs <$> getContents
         ["clause"]   -> putStr =<< printGraph . graphClause . parseDimacs <$> getContents
         _            -> error "Unknown argument, must be either \"variable\" or \"clause\"."
         

graphVariable :: Sat -> [(Int, Int)]
graphVariable s =
    s >>= mapClause
    where
        mapClause :: Clause -> [(Int, Int)]
        mapClause = pairs . map abs

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
printGraph g = unlines["digraph community {", unlines ["\t" ++ show a ++ " -> " ++ show b ++ ";" | (a, b) <- g], "}"]
        
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (x ,) xs ++ pairs xs

canonicalPair :: Ord a => (a, a) -> (a, a)
canonicalPair (a, b)
    | a <= b    = (a, b)
    | otherwise = (b, a)
    
nubSet :: Ord a => [a] -> [a]
nubSet = Set.toList . Set.fromList
