import Control.Applicative
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text (unpack, strip, pack)
import Numeric
import System.Environment

main :: IO ()
main = do
    (separator, skip) <- parseArgs <$> getArgs
    ls <- lines <$> getContents
    let (h, t) = splitAt (fromMaybe 0 skip) ls
    mapM_ putStrLn h
    let d = map (readLine separator) $ filter (not . null) $ map trim t
    mapM_ putStrLn $ map (showLine separator) $ map merge $ groupBy (testing fst) $ sortBy (comparing fst) d
    where
        merge g = (fst $ head g, map average $ transpose $ map snd g)
        
parseArgs :: [String] -> (String, Maybe Int)
parseArgs [separator] = (separator, Nothing)
parseArgs [separator, skip] = (separator, Just $ read skip)
parseArgs _ = error $ "Cannot find separator"
    
readLine :: String -> String -> (Double, [Double])
readLine separator line =
    case splitOn separator line of
         k:v -> (read k, map read v)
         _        -> error $ "Cannot split line \"" ++ line ++ "\" with separator '" ++ separator ++ "'"
         
showLine :: String -> (Double, [Double]) -> String
showLine separator (k, v) = intercalate separator $ map showDouble $ k : v

average :: [Double] -> Double
average xs = sum xs / genericLength xs

comparing :: Ord a => (b -> a) -> b -> b -> Ordering
comparing = on compare

testing :: Eq a => (b -> a) -> b -> b -> Bool
testing = on (==)

showDouble :: Double -> String
showDouble d = showFFloat Nothing d ""

trim :: String -> String
trim = unpack . strip . pack