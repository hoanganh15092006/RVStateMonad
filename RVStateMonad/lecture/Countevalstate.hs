module Countevalstate where

import           Control.Monad.State (State, get, put, evalState)

count :: State Int Int  -- 1 State Int Int nghĩa là state là Int, giá trị trả về cũng là Int
count = do
    n <- get
    put (n + 1)
    return n        
ex :: (Int, Int, Int)  -- giá trị trả về là bộ 3 Int
ex = evalState (do
    a <- count
    b <- count
    c <- count
    return (a,b,c)
    ) 0

main :: IO ()
main = do
    let (a, b, c) = ex   -- let chỉ để ràng buộc
    putStrLn ("a = " ++ show a ++ ", b = " ++ show b ++ ", c = " ++ show c)


