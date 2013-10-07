import Control.Applicative
import Sat

main :: IO ()
main = putStr =<< printDimacs . convert3cnf . parseDimacs <$> getContents