import Control.Applicative
import Control.Monad.Random
import qualified Data.Map as Map
import Sat
import System.Random.Shuffle

main :: IO ()
main = do
    s <- parseDimacs <$> getContents
    let v = [1..nVars s]
    w <- evalRandIO $ shuffleM v
    let x = Map.fromList $ zip v w
    putStr $ printDimacs [map (mapLit (x Map.!)) c| c <- s]
    
mapLit :: (Int -> Int) -> Lit -> Lit
mapLit f a
    | a < 0 = - f (-a)
    | a > 0 = f a
    | otherwise = error "Unknown 0 literal."