import Control.Applicative
import Sat

main :: IO ()
main = do
    s <- parseDimacs <$> getContents
    putStrLn $ show (count isHorn s) ++ " "  ++ show (count isAntiHorn s) ++ " " ++ show (count isHornOrAntiHorn s) ++ " " ++ show (nClauses s) ++ " " ++ show (nVars s)