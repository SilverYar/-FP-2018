module Task1_1 where

import Todo(todo)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTermAdd{ lBinaryTermAdd :: Term, rBinaryTermAdd :: Term } -- бинарная операция сложения
			| BinaryTermDif{ lBinaryTermDif :: Term, rBinaryTermDif :: Term } -- бинарная операция вычитания
			| BinaryTermMul{ lBinaryTermMul :: Term, rBinaryTermMul :: Term } -- бинарная операция умножения
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет

-- Задаем приоритет оператора бинарного сложения |+|: оператор лево-ассоциативный с 6-м приоритетом 
infixl 6 |+|
(|+|) :: Term -> Term -> Term
-- Определяем оператор бинарного сложения |+| в префиксном стиле 
(|+|) l r = BinaryTermAdd 1 r
-- Задаем приоритет оператора бинарной разности |-|: оператор лево-ассоциативный с 6-м приоритетом 
infixl 6 |-|
(|-|) :: Term -> Term -> Term
-- Определяем оператор бинарной разности |-| в префиксном стиле 
(|-|) l r = BinaryTermDif 1 r
-- Задаем приоритет оператора бинарного умножения |*|: 7-ой приоритетом 
infixl 6 |*|
(|*|) :: Term -> Term -> Term
-- Определяем оператор бинарного умножения |*| в префиксном стиле 
(|*|) l r = BinaryTermMul 1 r

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term

replaceVar var replacement Variable {varName = varName'} | varName' == var = replacement
replaceVar var replacement BinaryTermAdd{lBinaryTermAdd = l, rBinaryTermAdd = r}  = BinaryTermAdd  (replaceVar var replacement l) (replaceVar var replacement r)
replaceVar var replacement BinaryTermDif{lBinaryTermDif = l, rBinaryTermDif = r}  = BinaryTermDif (replaceVar var replacement l) (replaceVar var replacement r)
replaceVar var replacement BinaryTermMul{lBinaryTermMul = l, rBinaryTermMul = r} = BinaryTermMul (replaceVar var replacement l) (replaceVar var replacement r)
replaceVar _ _ a = a


-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate BinaryTermAdd { lBinaryTermAdd  = IntConstant 0, rBinaryTermAdd  = r}             = evaluate r
evaluate BinaryTermAdd { lBinaryTermAdd  = l,             rBinaryTermAdd  = IntConstant 0} = evaluate l
evaluate BinaryTermAdd { lBinaryTermAdd  = l,             rBinaryTermAdd  = r}             = (|+|) (evaluate l) (evaluate r)

evaluate BinaryTermDif { lBinaryTermDif = l,             rBinaryTermDif = IntConstant 0} = evaluate l
evaluate BinaryTermDif { lBinaryTermDif = l,             rBinaryTermDif = r}             = (|-|) (evaluate l) (evaluate r)

evaluate BinaryTermMul { lBinaryTermMul  = IntConstant 1, rBinaryTermMul = r}             = evaluate r
evaluate BinaryTermMul{ lBinaryTermMul  = l,             rBinaryTermMul  = IntConstant 1} = evaluate l
evaluate BinaryTermMul{ lBinaryTermMul  = l,             rBinaryTermMul  = r}             = (|*|) (evaluate l) (evaluate r)
evaluate a = a