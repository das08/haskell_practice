import Data.List  
import Data.Char
import qualified Data.Map as Map
  
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub  