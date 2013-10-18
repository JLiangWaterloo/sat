import Control.Applicative
import Sat

main :: IO ()
main = putStr =<< show . minipure . parseDimacs <$> getContents