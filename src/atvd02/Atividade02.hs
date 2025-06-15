-- Fundir dois vetores ordenados num vetor ordenado maior.
-- use casamento de padrões.
-- não use meios externos de ordenação.
-- use recursão.


merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys



-- implemente mergesort para 
-- ordenação do vetor u.
--   Use a função anterior.


mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs





-- usando fold implementar função que retorne 
-- a série de Fibonacci com n elementos.

fibo'list :: Int -> [Int]
fibo'list n = take n $ fst $ foldl f ([], (0,1)) [1..n]
  where
    f (acc, (a,b)) _ = (acc ++ [a], (b, a + b))

