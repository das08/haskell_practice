main = do
    putStrLn "Hello world, tell me your name:"
    name <- getLine
    putStrLn ("Hi " ++ name ++ "!")