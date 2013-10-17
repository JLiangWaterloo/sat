import Control.Applicative                                                                                                                                                          
import Data.Set (Set)                                                                                                                                                               
import qualified Data.Set as Set                                                                                                                                                    
import Sat                                                                                                                                                                          
import System.Environment                                                                                                                                                           
                                                                                                                                                                                    
main :: IO ()                                                                                                                                                                       
main = do
    keep <- Set.fromList . map read <$> getArgs
    s <- parseDimacs <$> getContents
    putStr $ printDimacs $ filter (within keep) s
    where
        within :: Set Int -> Clause -> Bool
        within set = all (`Set.member` set) . map abs
