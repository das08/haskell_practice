-- main = do     
--     c <- getChar  
--     if c /= ' '  
--         then do  
--             putChar c  
--             main  
--         else return ()  
import Control.Monad
import Data.Char

main = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  