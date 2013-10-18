import Control.Applicative
import Sat

main :: IO ()
main = putStr =<< printDimacs . pureBcp . parseDimacs <$> getContents