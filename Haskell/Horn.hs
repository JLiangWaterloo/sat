import Control.Applicative
import Sat
import System.Environment

main :: IO ()
main = do
    [a, b, c] <- getArgs
    putStr =<< printDimacs <$> make3Horn (read a) (read b) (read c)
