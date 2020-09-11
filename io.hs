-- main = do     
--     c <- getChar  
--     if c /= ' '  
--         then do  
--             putChar c  
--             main  
--         else return ()  
import Control.Monad
import Data.Char

main = forever $ do
    putStr "Enter something: "
    line <- getLine
    putStrLn $ map toUpper line