-- Atividade 03
-- Alunos: Jennifer Marques de Brito
-- Matrículas: 569710


data Key a = Key a 
    deriving (Show)


data Stack a = Stack [a] 
    deriving (Show)



push :: Stack b -> b -> Stack b
push (Stack lista) elemento = Stack (elemento : lista)


pop :: Stack b -> Stack b
pop (Stack [])     = error "Erro: pilha vazia"
pop (Stack (_:lista)) = Stack lista

top :: Stack a -> Key a
top (Stack [])     = error "Erro: pilha vazia"
top (Stack (x:_))  = Key x


empty :: Stack a -> Bool
empty (Stack []) = True
empty (Stack _)  = False


height :: Stack a -> Int
height (Stack lista) = len lista

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs



hex :: String -> String
hex str_decimal =
    let num_int = stringParaInt str_decimal 
    in intParaHex num_int                 


stringParaInt :: String -> Int
stringParaInt str = stringParaInt' 0 str
  where
    stringParaInt' acc [] = acc
    stringParaInt' acc (x:xs) = stringParaInt' (acc * 10 + charParaInt x) xs


charParaInt :: Char -> Int
charParaInt c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | otherwise            = error "Caractere inválido."


intParaHex :: Int -> String
intParaHex 0 = "0"
intParaHex n = inverteString (intParaHex' n)
  where
    intParaHex' 0 = ""
    intParaHex' num =
      let resto = num `rem` 16
          quociente = num `div` 16
      in intParaHexChar resto : intParaHex' quociente


intParaHexChar :: Int -> Char
intParaHexChar n
  | n < 10    = toEnum (fromEnum '0' + n)
  | n == 10   = 'A'
  | n == 11   = 'B'
  | n == 12   = 'C'
  | n == 13   = 'D'
  | n == 14   = 'E'
  | n == 15   = 'F'
  | otherwise = ' ' 


inverteString :: String -> String
inverteString str = inverte' str ""
  where
    inverte' [] acc = acc
    inverte' (x:xs) acc = inverte' xs (x:acc)