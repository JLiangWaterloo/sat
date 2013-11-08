import Control.Applicative
import Control.Monad.Random
import qualified Data.Map as Map
import Sat
import System.Environment
import System.Random.Shuffle

main :: IO ()
main = do
    args <- getArgs
    case args of
         ["variable"] -> putStr =<< printDimacs <$> (shuffleVars =<< parseDimacs <$> getContents)
         ["clause"]   -> putStr =<< printDimacs <$> (shuffleClauses =<< parseDimacs <$> getContents)
         _            -> error "Unknown argument, must be either \"variable\" or \"clause\"."
    
shuffleVars :: Sat -> IO Sat
shuffleVars s = do
    let v = [1..nVars s]
    w <- evalRandIO $ shuffleM v
    let x = Map.fromList $ zip v w
    return [map (mapLit (x Map.!)) c| c <- s]
    
shuffleClauses :: Sat -> IO Sat
shuffleClauses = shuffleM
    
mapLit :: (Int -> Int) -> Lit -> Lit
mapLit f a
    | a < 0 = - f (-a)
    | a > 0 = f a
    | otherwise = error "Unknown 0 literal."