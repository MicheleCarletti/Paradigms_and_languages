import Control.Monad

main = do
    --colors <- mapM (\a -> do
        --print $ "Which color do you associate with the number " ++ show a ++ "?"
        --color <- getLine
        --return color) [1,2,3,4]
    -- equivalent to:   (NB. Control.Monad.forM has switche parameters)
    colors <- forM [1,2,3,4] (\a -> do
        print $ "Which color do you associate with number " ++ show a ++ "?"
        color <- getLine
        return color)
    print "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM print colors