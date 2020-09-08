import Data.List  
import Data.Char
import qualified Data.Map as Map
import Geometry
  
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub  