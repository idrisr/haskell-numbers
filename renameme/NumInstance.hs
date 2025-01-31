{-# LANGUAGE InstanceSigs #-}

module Main where

data MyNum = A | B
    deriving (Show)

instance Num MyNum where
    (+) :: MyNum -> MyNum -> MyNum
    (+) A B = A
    (-) :: MyNum -> MyNum -> MyNum
    (-) A B = B
    (*) :: MyNum -> MyNum -> MyNum
    (*) A B = A
    negate :: MyNum -> MyNum
    negate A = B
    abs :: MyNum -> MyNum
    abs A = B
    abs B = B
    signum :: MyNum -> MyNum
    signum A = A
    fromInteger :: Integer -> MyNum
    fromInteger _ = A

square :: (Num a) => a -> a
square x = x * x

-- 2. Test the function with different numeric types.
main :: IO ()
main = do
    putStrLn $ "A + B = " ++ show (A + B)
    putStrLn $ "A - B = " ++ show (A - B)
    putStrLn $ "-A = " ++ show (-A)
    putStrLn $ "signum A = " ++ show (signum A)
    putStrLn $ "abs B = " ++ show (abs B)
    putStrLn $ "abs A = " ++ show (abs A)
