module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini []     = ini
foldl f ini (x:xs) = foldl f (f ini x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini []     = ini
foldr f ini (x:xs) = x 'f' foldr f ini xs 

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
    Just (a, b') -> a : unfoldr f b'
	Nothing      -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product []     = 1
product xs     = foldl (*) 1 xs

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes lst = foldr f [] lst where 
    f b acc = case b of
        Nothing -> acc
        Just x  -> (x: acc)

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal m = foldr (\s acc -> (s !! ((length s - 1) - (length acc)) : acc)) [] m

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p lst = foldr f [] lst
  where f x acc = if p x then acc else (x: acc)

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem x = foldl (\s x' -> if x' == x then True else s) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (\x -> if x < to then Just (x, x + step) else Nothing) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append lst1 lst2 = foldr (\x s -> x:s) lst2 lst1

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = reverse . (map reverse) . fst $ foldl group' ([], 0) lst
    where
        group'                 = (group'' n)
        group'' _ ([], 0) elem = ([[elem]], 1)
        group'' size' ((x : s), size) elem
            | size== size' = ([elem] : x : s, 1)
            | otherwise    = ((elem : x) : s, size + 1)