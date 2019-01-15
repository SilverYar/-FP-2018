module Task1_2 where

import Todo(todo)
import Prelude hiding (gcd, sin, cos)

factorial :: Double -> Double
factorial 0 = 1
factorial n | n <0 = error "arg must be >=0"
            | n >0 = n * factorial (n-1)	  
	  

Rad :: Double -> Double
Rad x | ((x >= (-2*pi)) && (x <= 2*pi)) = x
      | (x < (-2*pi))                   = Rad (x + 2*pi)
      | otherwise                       = Rad (x - 2*pi)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin'' x = [((-1) ** n)*(x ** (2*n + 1)) / (factorial (2*n + 1)) | n <- [0..]]
-- берем 10 первых членов ряда (мне кажется этой точности достаточно)
sin x = sum (take 10 (sin'' (Rad x)))

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos'' x = [((-1) ** n)*(x ** (2*n)) / (factorial (2*n)) | n <- [0..]]
cos x = sum (take 10 (cos'' (Rad x)))


-- наибольший общий делитель двух чисел		  
ggt :: Integer -> Integer -> Integer

ggt m n | m < 0 || n < 0  = error " m or n are less then 0"
        | m < n           = ggt n m
        | n == 0          = m
		| m == 0          = n
        | otherwise       = ggt n (m `mod` n)
		
-- существует ли полный целочисленный квадрат в диапазоне [from, to)?		
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = (roundsqrt(to - 1) - roofsqrt from) >= 0
  where
    roundsqrt = floor . sqrt . fromIntegral
    roofsqrt = ceiling . sqrt . fromIntegral
	
	
-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x | (length [n | n <- [2 .. x-1], mod x n == 0]) > 0 = False
          | otherwise = True
