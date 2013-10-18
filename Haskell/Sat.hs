module Sat where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Control.Monad.Supply
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

type Sat = [Clause]
type Clause = [Lit]
type Lit = Int

-- https://en.wikipedia.org/wiki/Conflict_Driven_Clause_Learning
wiki :: [Clause]
wiki = [
    [1, 4],
    [1, -3, -8],
    [1, 8, 12],
    [2, 11],
    [-7, -3, 9],
    [-7, 8, -9],
    [7, 8, -10],
    [7, 10, -12]]

nClauses :: Sat -> Int
nClauses = length

nVars :: Sat -> Int
nVars = foldl max 0 . map abs . concat

-- Takes into account gaps int the variable numbering.
nVarsReal :: Sat -> Int
nVarsReal = Set.size . Set.fromList . map abs . concat

isHorn :: Clause -> Bool
isHorn c = count isPos c <= 1

isAntiHorn :: Clause -> Bool
isAntiHorn c = count isNeg c <= 1

isHornOrAntiHorn :: Clause -> Bool
isHornOrAntiHorn c = isHorn c || isAntiHorn c

isAtLeastNeg :: Clause -> Bool
isAtLeastNeg c = count isNeg c >= 1

is1Sat :: Clause -> Bool
is1Sat [_] = True
is1Sat _ = False

is2Sat :: Clause -> Bool
is2Sat [_, _] = True
is2Sat _ = False

isComplementary :: Clause -> Bool
isComplementary (x:xs) = -x `elem` xs || isComplementary xs
isComplementary []     = False

isUnit :: Clause -> Bool
isUnit [_] = True
isUnit _   = False

isPos :: Lit -> Bool
isPos 0 = undefined
isPos x = x > 0

isNeg :: Lit -> Bool
isNeg 0 = undefined
isNeg x = x < 0

varsWithSameSign :: Sat -> [Int]
varsWithSameSign s =
    [l | l <- Set.toList lits, (-l) `Set.notMember` lits]
    where
        lits = Set.fromList $ concat s
        

bcp :: [Clause] -> [Clause]
bcp = map Set.toList . bcp' . map Set.fromList

bcp' :: [Set Lit] -> [Set Lit]
bcp' s =
    if null unitClauses then s else unitClauses ++ bcp' (mapMaybe unitPropagate clauses)
    where
        (unitClauses, clauses) = partition ((== 1) . Set.size) s
        units = Set.fromList $ map (Set.elemAt 0) unitClauses
        negUnits = Set.map negate units
        unitPropagate c
            | isComplementary $ Set.toList c = Nothing
            | not $ Set.null $ units `Set.intersection` c = Nothing
            | otherwise                                   = Just $ c `Set.difference` negUnits

pureBcp :: [Clause] -> [Clause] 
pureBcp = map Set.toList . pureBcp' . map Set.fromList
            
pureBcp' :: [Set Lit] -> [Set Lit]
pureBcp' s =
    if null unitClauses && null pures then s else unitClauses ++ map Set.singleton pures ++ pureBcp' (mapMaybe unitPropagate clauses)
    where
        (unitClauses, clauses) = partition ((== 1) . Set.size) s
        lits = Set.unions s
        pures = [l | l <- Set.toList lits, (-l) `Set.notMember` lits]
        units = Set.fromList $ map (Set.elemAt 0) unitClauses ++ pures
        negUnits = Set.map negate units
        unitPropagate c
            | isComplementary $ Set.toList c = Nothing
            | not $ Set.null $ units `Set.intersection` c = Nothing
            | otherwise                                   = Just $ c `Set.difference` negUnits

parseDimacs :: String -> Sat
parseDimacs = map (map read . init) . dropWhile ((== "p") . head) . dropWhile ((== "c") . head) . filter (not . null) . map words . lines

printDimacs :: Sat -> String
printDimacs s =
    "p cnf " ++ show (nVars s) ++ " " ++ show (nClauses s) ++ "\n" ++ concatMap printClause s
    where
        printClause c = intercalate " " (map show c) ++ " 0\n"
        
convert3cnf :: [Clause] -> [Clause]
convert3cnf s = concat $ evalSupply (mapM convert3cnf' s) [nVars s + 1..]

convert3cnf' :: Clause -> Supply Lit [Clause]
convert3cnf' []           = return [[]]
convert3cnf' [a]          = return [[a, a, a]]
convert3cnf' [a, b]       = return [[a, a, b]]
convert3cnf' [a, b, c]    = return [[a, b, c]]
convert3cnf' (a : b : cs) = do
    s <- supply
    ([a, b, s] :) <$> convertKcnf cs s
    where
        convertKcnf [x, y] r = return [[-r, x, y]]
        convertKcnf (x:xs) r = do
            s <- supply
            ([-r, x, s] :) <$> convertKcnf xs s
        convertKcnf [] _     = undefined
    

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

makeCmty :: Int -> Int -> Int -> Double -> IO Sat
makeCmty vars clauses cmtys q =
    evalRandIO $ replicateM clauses make3Clause
    where
        make3Clause = do
            cmty <- getRandomR (1, cmtys)
            a <- makeCmtyLit cmty
            r1 <- getRandomR (0, 1)
            b <- if r1 < q then makeCmtyLit cmty else makeLit
            r2 <- getRandomR (0, 1)
            c <- if r2 < q then makeCmtyLit cmty else makeLit
            return [a, b, c]
        makeLit = getRandomR (1, vars) >>= makeSign
        makeCmtyLit cmty = do
            val <- getRandomR (0, cmtySize)
            makeSign $ val * cmtys + cmty
        makeSign v = do
            s <- getRandom
            if s then return v else return (-v)
        cmtySize = vars `div` cmtys

make3Horn :: Int -> Int -> Double -> IO Sat
make3Horn vars clauses p =
    evalRandIO $ replicateM nHorn make3HornClause <++> replicateM nAntiHorn make3AntiHornClause
    where
    nHorn = round $ fromIntegral clauses * p
    nAntiHorn = clauses - nHorn
    make3HornClause = do
        a <- getRandomR (1, vars)
        b <- getRandomR (1, vars)
        c <- getRandomR (1, vars)
        z <- getRandomR (1::Int, 4)
        return $ case z of
             1 -> [a, -b, -c]
             2 -> [-a, b, -c]
             3 -> [-a, -b, c]
             _ -> [-a, -b, -c]
    make3AntiHornClause = map negate <$> make3HornClause
    (<++>) = liftM2 (++)
