module Part2 where

import Part2.Types

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 RED = 'R'
prob6 GREEN = 'G'
prob6 BLUE = 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 cp = getValue cp >= 0 && getValue cp <= 255

getValue :: ColorPart -> Int
getValue cp = case cp of
  Red x -> x
  Green x -> x
  Blue x -> x

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 c cp = case cp of
  Red x -> c { red = red c + x }
  Blue x -> c { blue = blue c + x }
  Green x -> c { green = green c + x }

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 cp = case cp of
  Red x -> x
  Green x -> x
  Blue x -> x

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 c
  | red c > green c && red c > blue c = Just (Red (red c))
  | green c > red c && green c > blue c = Just (Green (green c))
  | blue c > red c && blue c > green c = Just (Blue (blue c))
  | otherwise = Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 t = root t + sum (left t) + sum (right t)
  where 
    sum t = maybe 0 prob11 t

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 t = all (< (root t)) (leftValues (left t)) && all (>= (root t)) (rightValues (right t))

leftValues t = case t of
  Nothing -> []
  Just value -> [(root value)] ++ leftValues (left value)

rightValues t = case t of
  Nothing -> []
  Just value -> [(root value)] ++ leftValues (left value)

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 t = maybe t leftRotation (right t)
  where
    leftRotation s = s { left = Just oldRoot }
      where
        oldRoot = t { right = left s }

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 t = maybe t rightRotation (left t)
  where 
    rightRotation s = s { right = Just oldRoot }
      where
        oldRoot = t { left = right s }

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"
