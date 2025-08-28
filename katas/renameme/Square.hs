module Main where

square :: (Num a) => a -> a
square x = x * x

-- 2. Test the function with different numeric types.
main :: IO ()
main = do
    let intResult = square (5 :: Int) -- Using Int
    let integerResult = square (5 :: Integer) -- Using Integer
    let floatResult = square (5.0 :: Float) -- Using Float
    let doubleResult = square (5.0 :: Double) -- Using Double

    -- Print the results
    putStrLn $ "Square of 5 (Int): " ++ show intResult
    putStrLn $ "Square of 5 (Integer): " ++ show integerResult
    putStrLn $ "Square of 5.0 (Float): " ++ show floatResult
    putStrLn $ "Square of 5.0 (Double): " ++ show doubleResult
