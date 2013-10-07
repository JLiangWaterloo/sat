import Control.Applicative
import Data.List
import Data.List.Split
import System.Environment

main :: IO ()
main = do
    [separator] <- getArgs
    putStr =<< unlines . map (intercalate separator) . transpose . map (splitOn separator) . lines <$> getContents