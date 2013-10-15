import Control.Applicative
import Sat

main :: IO ()
main = do
    s <- parseDimacs <$> getContents
    putStrLn $ show (count isHorn s) ++ " "  ++ show (count isAntiHorn s) ++ " " ++ show (count is1Sat s) ++ " " ++ show (count is2Sat s) ++ " "  ++ show (nClauses s) ++ " " ++ show (nVars s)
