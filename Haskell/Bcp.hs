import Control.Applicative
import Sat

main :: IO ()
main = putStr =<< printDimacs . bcp . parseDimacs <$> getContents