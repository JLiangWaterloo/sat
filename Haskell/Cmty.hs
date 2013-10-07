import Control.Applicative
import Sat
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
         [a, b, c, d] -> putStr =<< printDimacs <$> makeCmty (read a) (read b) (read c) (read d)
         _            -> error "Unknown argument s, must be <vars> <clauses> <cmtys> <q>."
