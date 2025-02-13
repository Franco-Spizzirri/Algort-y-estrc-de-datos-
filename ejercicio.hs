apply :: (a->b) -> a -> b -- toma una funcion cualquiera (2+), (2*) y lo aplica a un valor x que lo pongo por afuera
apply f x = f x 

identidad :: (Num a)=> a -> a
identidad x = x

first :: (a,b)->a --- aca declaro la funcion
first (x,_) = x --- aca defino la funcion 

derive :: (Double->Double)->Double->Double->Double
derive f x h = (f(x+h) - f(x))/h 

sign :: (Ord a, Num a)=> a -> a 
sign x 
  | x > 0 = 1 
  | x < 0 = -1 
  | otherwise = 0

abs'::(Ord a, Num a)=>a->a 
abs' x
   | x > 0 = x
   | x < 0 =(-x)
   | otherwise = 0 

pot :: Int->Double->Double 
pot x y = y ^ x

xor :: Bool->Bool->Bool
xor a b = (a || b) && not (a && b)

max' :: Int->Int->Int->Int
max' a b c = 
    if a>b && a>c 
    then a
    else if b>c 
        then b 
        else c 

swap:: (a,b)->(b,a)
swap (x,y) = (y,x)


--2. Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:

--a) (Int → Int) → Int            Int->Int->Int



-- b) Int → (Int → Int)     es lo mismo que hacer Int->Int->Int las -> me indican la cant de argumentos que necesito
mult :: Int -> (Int->Int)    --en este caso mult le paso los argumentos asi no mas 
mult x y = x * y

add :: Int -> (Int->Int)
add x y = x + y 

-- c) (Int → Int) → (Int → Int)    




-- d) Int → Bool                     el booleano me indica si es true o false, entonces puedo crear una funcion en la que me deje poner un entero y me de un valor verdadero o falso
esPar :: Int -> Bool
esPar x = (x `mod` 2) == 0         -- el x es el entero que yo voy a ingresar en la terminal y lo q esta luego del igual es el booleano que tiene q retornar. Uso x mod (funcion q viene predefinida dentro de haskell) y lo uso con dos comparandolo si es = a 0, como si fuera en c. 

esPositivo :: Int -> Bool
esPositivo x = x > 0

esNegativo :: Int -> Bool
esNegativo x = x < 0

-- e) Bool → (Bool → Bool)
funcionxor :: Bool->(Bool->Bool)
funcionxor a b = (a || b) && not (a && b)

funcionand :: Bool -> (Bool -> Bool)
funcionand a b = a && b


-- f) (Int,Char) → Bool     (Int, Char) es una Tupla ya que cada elemento puede ser distinto
esParyVocal :: (Int,Char) -> Bool
esParyVocal (x,y) = ((x `mod` 2) == 0) && (y `elem` "aeiouAEIOU") -- en la terminal hay q poner la vocal con ' '

esNegativoyMayuscula :: (Int,Char) -> Bool
esNegativoyMayuscula (x,y) = (x < 0) && (y `elem` ['A'..'Z']) -- los corchetes me sirven para indicar que tome un conjunto q va de  A a Z. 

-- g) (Int,Int) → Int
suma :: (Int, Int) -> Int 
suma (x, y) = x + y

multiplicacion :: (Int, Int) -> Int     -- en multiplicacion tengo q pasarle los argumentos () ya que es el formato en q me lo pide la funcion. Atento a esto
multiplicacion (x, y) = x * y

-- h) Int → (Int,Int)           en este caso es un entero q devuelve una tupla
doblePar :: Int -> (Int,Int)  
doblePar x = (x, 2*x)

cuadradoNum :: Int -> (Int,Int)
cuadradoNum x = (x, x*x)        -- aca va a ser en la tupla primero la x y luego el cuadrado

-- i) a → Bool
esPosit :: (Ord a, Num a) => a -> Bool
esPosit x = x > 0
noesCero :: (Num a, Eq a) => a -> Bool
noesCero x = x /= 0

-- j) a → a
iden :: (Num a)=> a -> a
iden x = x

valorabs'::(Ord a, Num a)=> a -> a 
valorabs' x
   | x > 0 = x
   | x < 0 =(-x)
   | otherwise = 0


--    3. Reescribir cada una de las siguientes definiciones sin usar let, where o if:
   
-- a) f x = let (y,z) = (x,x) in y  
-- f x -> f :: a->b
-- el let indica que a la tupla (x,x) le asigmanos los valores de la tupla (y,z). O sea y=x y z=x. 
-- la expresion luego del in es la que se evaluara como el resultado de la funcion. Al ser simplemente Y e Y previamente asignado a x por el let, entonces la salida es simplemente x. 
f1 :: a -> a 
f1 x = x
-- b) greater (x,y) = if x > y then True else False
greater :: Ord a => a -> a -> Bool  -- es a ya que estamos tomando argumentos del mismo tipo (int, char, etc). Al estar comparando en orden ponemos Ord a, aclaramos q hacemos comparaciones en orden.
greater x y = x > y

-- c) f (x,y) = let z = x + y in g (z,y) where g (a,b) = a − b
--f2 :: (Num c) => (c, c) -> c
--f2 (x, y) = f tiene dos variables x e y. El let me define a z como la suma de x e y.
--   let z = x + y in g (z, y) Aca definimos g q toma a z (x+y) y al valor Y del conjunto de f.
--  where g (a, b) = a - b Aca declara a g como la resta de A y B
--g seria = a hacer g(x+y,y). Que luego en la declaracion seria: (x+y) - y = x ya que el Y positivo y el negativo se cancelan. 
--Entonces si al final la funcion f va a ser = x, quiere decir que F(x,y) = x. 
 
f2 :: a -> b -> a -- esto asi ya q f toma dos argumentos (por ser una tupla) y me devuelve el primero (el x). 
f2 x y = x


-- 4. Suponiendo que f y g tienen los siguientes tipos

-- f :: c → d
-- g :: a → b → c
-- y sea h definida como
-- h x y = f (g x y)
-- Determinar el tipo de h e indicar cuales de las siguientes definiciones de h son equivalentes a la
-- dada:
-- h = f · g   
-- h x = f · (g x)  esta no. Toma un solo argumento x por lo que da como resultado b -> c y al aplicarlo a f seria: b -> d. Lo cual no es correcto con el tipo de h. 
-- h x y = (f · g) x y    esta si ya que es lo mismo hacer f (g x y) si cambiamos el orden del producto
-- ¿Cu´al es el tipo de la funcion ( · )?


-- Cuando determinamos el tipo de h primero hay q verla con detenimiento. 
-- h toma un X y un Y, que hace: una composicion donde primero aplica g (que toma dos argumentos a y b, por eso va primero si no no podria funcionar) devolviendo un c. 
--Ya que g :: a → b → c. (G toma dos argumentos A y B que me devuelven un C) Luego, aplica f a ese c que termino quedando ya que f :: c → d (F toma 1 argumento C que me devuelve un D). 
-- Entonces esto nos termina devolviendo un D. 
-- Por lo tanto, el tipo de H deberia tener dos argumentos y q esto me devuelva un d. 

-- f3 :: c -> d
-- g :: a -> b -> c
-- h :: a -> b -> d
-- h x y = f (g x y)


-- 5. Definir una funcion que determine si un año es bisiesto o no, de acuerdo a la siguiente
-- definicion:
-- año bisiesto 1. m. El que tiene un dıa mas que el año comun, añadido al mes de febrero. Se
-- repite cada cuatro años, a excepcion del ultimo de cada siglo cuyo numero de centenas no
-- sea multiplo de cuatro. (Diccionario de la Real Academia Española, 22◦
-- ed.)
-- ¿Cual es el tipo de la funcion definida?

-- Si el año es divisible entre 4 o es divisible entre 400 y no es divisible entre 100 , entonces el año es bisiesto.

esBisiesto :: Int -> Bool
esBisiesto año = 
   if año `mod` 400 == 0         then True 
   else if año `mod` 100 == 0    then False
   else if año `mod` 4 == 0      then True
   else False 


-- 6. Sin usar funciones predefinidas, defina recursivamente las siguientes funciones y determine su
-- tipo m´as general:

-- a) suma, que suma todos los elementos de una lista de n´umeros

funcionSuma :: Num a =>[a] -> a  -- cuando me pide lista de numeros puede ser de cualquier tipo(int, float, etc) entonces al poner q es de tipo num estamos diciendo q puede ser de cualquiera de esos tipos. 
funcionSuma [] = 0
funcionSuma (x:xs) = x + funcionSuma xs  --la recursividad aca lo aplicamos a el resto q serian los xs

-- b) alguno, que devuelve True si alg´un elemento de una lista de valores booleanos es True, y
-- False en caso contrario

alguno :: [Bool] -> Bool
alguno [] = False -- si la lista esta vacia es false
alguno (x:xs) = x || alguno xs -- si el primer elemento o alguno de los siguientes es true, duevuelve true. Si todos son false o la lista esta vacia es false. 


-- c) todos, que devuelve True si todos los elementos de una lista de valores booleanos son True,
-- y False en caso contrario

todos :: [Bool] -> Bool
todos [] = True -- ya que si llega al final y ve lista vacia va a tener un False, por ende va a poner todos en false. 
todos (x:xs) = x && todos xs 

-- d) codes, que dada una lista de caracteres, devuelve la lista de sus ordinales

codes :: [Char] -> [Int]
codes [] = []  -- Caso base: Si la lista está vacía, devolvemos una lista vacía
codes (x:xs) = fromEnum x : codes xs  -- Convertimos el primer carácter en su ordinal y aplicamos recursión al resto de la lista
                                       --La función toma el primer elemento de la lista (x) y lo convierte a su valor ordinal usando fromEnum. Este valor ordinal es un número entero que corresponde al valor de la tabla de caracteres (ASCII o Unicode, dependiendo del sistema).

-- e) restos, que calcula la lista de los restos de la divisi´on de los elementos de una lista de
-- n´umeros dada por otro n´umero dado

restos ::  [Int] -> Int -> [Int]
restos [] _ = []  -- Caso base: si la lista está vacía, devolvemos una lista vacía
restos (x:xs) n = (x `mod` n) : restos xs n  -- Calculamos el resto de x dividido por n y aplicamos recursión al resto de la lista

--El operador mod solo está disponible para tipos que sean instancias de Integral, como Int y Integer, por lo que necesitamos especificar que el tipo a debe ser de la clase Integral en la firma de tipo.


-- f) cuadrados, que dada una lista de n´umeros, devuelva la lista de sus cuadrados

cuadrados :: Num a => [a] -> [a]
cuadrados [] = []
cuadrados (x:xs) = x^2 : cuadrados xs -- los : lo usas para concatenar

-- g) longitudes, que dada una lista de listas, devuelve la lista de sus longitudes

longitud1 :: [a] -> Int
longitud1 [] = 0                  -- calcula la longitud de una lista
longitud1 (x:xs) = 1 + longitud1 xs 

--te dice la cantidad de elementos que hay en cada lista

longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = longitud1 x : longitudes xs      -- llamo primero a la funcion longitud1 para analizar la longitud de la lista y longitudes analiza la longitud de las listas.

-- h) orden, que dada una lista de pares de n´umeros, devuelve la lista de aquellos pares en los
-- que la primera componente es menor que el triple de la segunda

orden :: (Num a, Ord a) => [(a,a)] -> [(a,a)]
orden [] = []
orden ((x,y):xs) = if x < y * 3 then (x,y) : orden xs -- si el primero es menor que el segundo x3 entonces va a meter ese par ordenado
                  else orden xs -- de no ser asi, no lo pone y llama a recursion para probar con el resto de pares ordenados. 

-- i) pares, que dada una lista de enteros, devuelve la lista de los elementos pares

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) = if x `mod` 2 == 0 then ( x : pares xs) 
               else pares xs 


-- j) letras, que dada una lista de caracteres, devuelve la lista de aquellos que son letras
-- (min´usculas o may´usculas)

letras :: [Char] -> [Char]
letras [] = []
letras (x:xs)  
   | (x >= 'a' && x<= 'z') || (x >= 'A' && x<= 'Z' ) = x : letras xs 
   |otherwise = letras xs 

-- k) masDe, que dada una lista de listas xss y un n´umero n, devuelve la lista de aquellas listas
-- de xss con longitud mayor que n


masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (x:xs) n = if longitud1 x > n -- en caso de no tener creada la funcion longitud1, hay q hacerla. 
                  then x : masDe xs n
                  else masDe  xs n 


-- 7. El producto escalar de dos listas de enteros de igual longitud es la suma de los productos de
-- los elementos sucesivos (misma posici´on) de ambas listas. Usando listas por comprensi´on defina
-- una funci´on scalarproduct que devuelva el producto escalar de dos listas.
-- Sugerencia: Usar las funciones zip y sum.
-- • zip [1,2,3] [4,5,6] = [(1,4),(2,5),(3,6)]
-- • sum [1,2,3] = 6


-- El producto escalar de dos listas de enteros de igual longitud es la suma de los productos de los elementos correspondientes de ambas listas. Es decir:
-- Si tenemos dos listas a = [a1, a2, a3] y b = [b1, b2, b3], el producto escalar es
-- a1 × b1 + a2 × b2 + a3 × b3
-- Por ejemplo, si a = [1, 2, 3] y b = [4, 5, 6], el producto escalar sería:
-- (1×4) + (2×5) + (3×6) = 4 + 10 + 18 = 32


-- En Haskell, una lista por comprensión es una forma concisa y poderosa de construir listas. La sintaxis general es:
-- [expresión | condición]
-- Por ejemplo:
-- [ x * x | x <- [1, 2, 3]]  => Resultado: [1, 4, 9]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct a b = sum [x * y | (x, y) <- zip a b]

-- el zip me combina las dos listas a y b en dos listas de tuplas. 
--en la lista por comprension recorre la tupla creada por zip y calcula x*y y el resultado es una lista de productos de cada par de elementos. 
--sum suma todos los elementos de la lista generada por la lista por comprension, siendo los productos de los elementos correspondientes. 


-- 8. Definir las siguientes funciones usando listas por comprensi´on:

-- a) divisors, que dado un entero positivo x devuelve la lista de los divisores de x (y la lista vac´ıa
-- si el entero no es positivo).

divisors :: Int -> [Int]
divisors x = if x > 0
             then [y | y <- [1..x], x `mod` y == 0]
             else []

 -- otra manera
-- divisors x
-- | x > 0 = [y | y <- [1..x], x mod y ==0]
-- | otherwise = []

-- b) matches, que dados un entero x y una lista de enteros descarta de la lista los elementos distintos
-- a x.

matches :: Int -> [Int] -> [Int]
matches x lista = [y | y <- lista, y == x]

-- c) unique, que dada una lista xs de enteros, devuelve la lista con los elementos no repetidos de
-- xs.
cont:: Int -> [Int] -> Int
cont _ [] = 0
cont x (y:ys)
 | x==y    = 1 + cont x ys -- para ver cuantas veces aparece un numero en una lista
 | otherwise = cont x ys
 --funcionamiento de cont: si x == y, suma 1, y luego se fija si aparece mas veces en el resto de elementos
 -- si x /= y, se fija cuantas veces aparece x en el resto de la lista

unique:: [Int] -> [Int]
unique l = [x | x <- l, cont x l == 1] -- el filtro es que solo aparezcan en la nueva lista los numeros que aparecen solo una vez


-- d) cuadrupla, que dados cuatro enteros a, b, c y d tales que 0 < a, b, c, d, ≤ 100, devuelve las
-- cuadruplas (a, b, c, d) que cumplen a^2 + b^2 = c^2 + d^2, es decir una tupla de 4 enteros. 

cuadrupla :: Int -> Int -> Int -> Int -> [(Int, Int, Int, Int)]
cuadrupla aMax bMax cMax dMax =
  [ (a, b, c, d) | a <- [1..aMax], b <- [1..bMax], c <- [1..cMax], d <- [1..dMax], a^2 + b^2 == c^2 + d^2 ]

--[1..aMax]: Genera una lista de enteros desde 1 hasta aMax para a, lo mismo para b, c y d.
--a^2 + b^2 == c^2 + d^2: Esta es la condición que debe cumplirse para incluir la tupla (a, b, c, d) en el resultado.

-- 9. Definir el tipo de datos Direction cuyos valores describan los puntos cardinales. Definir la
-- funci´on move que dado un punto en el plano (representado como un par de enteros) y una direcci´on
-- devuelva el punto que se obtiene el desplazarse una unidad en dicha direcci´on.

-- data lo usamos para "definir" un tipo de dato. 
--tipos enumerados: 
data Direccion = Norte | Sur | Este | Oeste deriving Show

move :: (Int, Int) -> Direccion -> (Int,Int)
move (x,y) Norte = (x,y +1) -- se mueve 1 hacia arriba en la y
move (x,y) Sur = (x,y -1)  -- hacia abajo 1 en la 1 
move (x,y) Este = (x +1,y) -- hacia la derecha 1 en x
move (x,y) Oeste = (x -1,y) -- hacia la izquierda 1 en x 

--Pattern matching: 
data Estacion = Primavera | Verano | Otoño | Invierno
data Temperatura = Frio | Templado | Caliente
tiempo :: Estacion -> Temperatura
tiempo Primavera = Caliente
tiempo Verano = Caliente
tiempo Otoño = Templado
tiempo Invierno = Frio

data Persona = Pers String Int Int deriving Show
juan :: Persona 
juan = Pers "Juan Lopez" 39235478 23 
verPersona :: Persona -> String
verPersona (Pers nombre docu edad) = "Persona, Nombre:" ++ show nombre ++ ", Edad:" ++ show edad ++ ",DNI:" ++ show docu

data Personas = Person { nombre :: String, edad :: Int} deriving Show
p1 :: Personas 
p1 = Person {nombre = "Fernando", edad = 22}

-- 10.
-- a) Definir las operaciones de suma y producto m´odulo 2 para el tipo
-- data DigBin = Cero |Uno
-- b) Definir las operaciones de suma binaria, producto por dos, cociente y resto de la divisi´on por
-- dos para el tipo:
-- type NumBin = [Digbin]
-- donde convenimos que el primer elemento de las lista de d´ıgitos es el d´ıgito menos significativo
-- del n´umero representado.
-- c) Redefinir las funciones del item anterior, observando una convenci´on opuesta.
-- d) Definir funciones que multipliquen nA˜ omeros binarios de acuerdo a las dos convenciones.

-- Definimos el tipo DigBin para los dígitos binarios
data DigBin = Cero | Uno deriving (Show, Eq)

-- Al agregar deriving Eq, Haskell genera automáticamente una instancia de Eq para el tipo DigBin, lo que significa que Cero == Cero devuelve True, Cero == Uno devuelve False, y lo mismo para Uno.
-- Esto hace que sea posible usar el operador == en el código sin necesidad de definir explícitamente la función eq para el tipo DigBin.


-- Suma módulo 2 (XOR)
sumatoriabinario :: DigBin -> DigBin -> DigBin
sumatoriabinario Cero Cero = Cero
sumatoriabinario Cero Uno  = Uno
sumatoriabinario Uno  Cero = Uno
sumatoriabinario Uno  Uno  = Cero

-- Producto módulo 2 (AND)
productobinario :: DigBin -> DigBin -> DigBin
productobinario Cero _ = Cero
productobinario _ Cero = Cero
productobinario Uno  Uno = Uno

-- Tipo NumBin para representar números binarios como listas de DigBin
type NumBin = [DigBin]

-- Suma binaria 
sumaBinaria :: NumBin -> NumBin -> NumBin
sumaBinaria xs ys = sumaBinaria' xs ys Cero  -- Comienza con el acarreo como Cero

-- Función auxiliar para la suma binaria con acarreo
sumaBinaria' :: NumBin -> NumBin -> DigBin -> NumBin
sumaBinaria' [] [] Cero = []
sumaBinaria' [] [] Uno  = [Uno]
sumaBinaria' (x:xs) [] acarreo = sumaBinaria' xs [Cero] acarreo  -- Si ys está vacío, consideramos un 0. Esta línea maneja el caso cuando una de las listas es más corta que la otra
sumaBinaria' [] (y:ys) acarreo = sumaBinaria' [Cero] ys acarreo  -- Si xs está vacío, consideramos un 0
sumaBinaria' (x:xs) (y:ys) acarreo =
    let sumaBit = sumatoriabinario (sumatoriabinario x y) acarreo
        nuevoAcarreo = if sumaBit == Uno then Uno else Cero
    in sumaBit : sumaBinaria' xs ys nuevoAcarreo  -- Recursivamente sumar el siguiente bit

--Primer argumento (NumBin): Representa el primer número binario que estamos sumando. Es una lista de dígitos binarios.
--Segundo argumento (NumBin): Representa el segundo número binario que estamos sumando. También es una lista de dígitos binarios.
--Tercer argumento (DigBin): Representa el acarreo de la suma. Al inicio, cuando no hay acarreo (es la primera suma), se pasa como Cero. Luego, a medida que vamos sumando los bits, el acarreo puede cambiar a Uno si la suma de dos bits da 2 (es decir, 1 + 1 = 2, lo que produce un acarreo de 1).

-- Producto por dos (equivalente a desplazamiento a la izquierda)
productoPorDos :: NumBin -> NumBin
productoPorDos xs = xs ++ [Cero]

-- Cociente de la división por dos (eliminar el bit menos significativo)
cocientePorDos :: NumBin -> NumBin
cocientePorDos [] = []
cocientePorDos [x] = []  -- Si solo hay un dígito, el cociente es vacío
cocientePorDos (x:xs) = x : cocientePorDos xs

-- Resto de la división por dos (el último bit)
restoPorDos :: NumBin -> DigBin
restoPorDos [] = Cero
restoPorDos [x] = x
restoPorDos (_:xs) = restoPorDos xs

-- Suma binaria (convención opuesta, bit más significativo al principio)
sumaBinariaOpuesta :: NumBin -> NumBin -> NumBin
sumaBinariaOpuesta xs ys = reverse (sumaBinaria' (reverse xs) (reverse ys) Cero)

-- Producto por dos (convención opuesta)
productoPorDosOpuesta :: NumBin -> NumBin
productoPorDosOpuesta xs = [Cero] ++ xs  -- Desplazar agregando cero al principio

-- Cociente de la división por dos (convención opuesta)
cocientePorDosOpuesta :: NumBin -> NumBin
cocientePorDosOpuesta [] = []
cocientePorDosOpuesta [x] = []  -- Si solo hay un dígito, el cociente es vacío
cocientePorDosOpuesta (x:xs) = cocientePorDosOpuesta xs

-- Resto de la división por dos (convención opuesta)
restoPorDosOpuesta :: NumBin -> DigBin
restoPorDosOpuesta [] = Cero
restoPorDosOpuesta [x] = x
restoPorDosOpuesta (_:xs) = restoPorDosOpuesta xs


-- Multiplicación binaria (primera convención)
multiplicacionBinaria :: NumBin -> NumBin -> NumBin
multiplicacionBinaria xs ys = foldr sumaBinaria [] (map (\y -> if y == Uno then productoPorDos xs else []) ys)

-- Multiplicación binaria (convención opuesta)
multiplicacionBinariaOpuesta :: NumBin -> NumBin -> NumBin
multiplicacionBinariaOpuesta xs ys = foldr sumaBinariaOpuesta [] (map (\y -> if y == Uno then productoPorDosOpuesta xs else []) ys)



-- 11. Dada las siguientes representaciones de los ´arboles generales y de los ´arboles binarios

-- data GenTree a = EmptyG | NodeG a [GenTree a]
-- data BinTree a = EmptyB | NodeB (BinTree a) a (BinTree a)
-- definir una funci´on que dado un ´arbol general, lo transforme en un ´arbol binario de la siguiente
-- manera:
-- Cada nodo NodeG en orden en el ´arbol general corresponde a un nodo NodeB en el ´arbol binario;
-- el hijo de la izquierda de NodeB es el nodo correspondiente al primer hijo de NodeG, y el hijo
-- derecho de NodeB es el nodo correspondiente al siguiente hermano de NodeG, es decir, el pr´oximo
-- nodo en orden entre los hijos de los padres de NodeG.

-- El primer arbol izq del binario es el primer hijo del general. El derecho seria el segundo hijo del general. 

data GenTree a = EmptyG | NodeG a [GenTree a] deriving Show -- definimos el arbol vacio | nodo raiz [lista de subarboles izquierdo y derecho]
data BinTree a = EmptyB | NodeB (BinTree a) a (BinTree a) deriving Show -- definimos el arbol vacio | nodo con (nodo izquierdo) la raiz (nodo derecho)

-- Función para transformar un árbol general en un árbol binario
genToBin :: GenTree a -> BinTree a -- recibe un arbol general y devuelve un binario
genToBin EmptyG = EmptyB -- un arbol general vacio es igual a un binario vacio
genToBin (NodeG x []) = NodeB EmptyB x EmptyB  -- Si no tiene hijos, es un nodo sin hijos en el árbol binario
genToBin (NodeG x (h:hs)) = NodeB (genToBin h) x (transformaHermanos hs x) -- transforma en binario primer elemento (hijo izq) y el hijo derecho se forma llamando recursivamente a transformar hermanos
  where
    -- Función auxiliar para transformar los hermanos del nodo
    transformaHermanos :: [GenTree a] -> a -> BinTree a
    transformaHermanos [] _ = EmptyB  -- No hay más hermanos
    transformaHermanos (h:hs) x = NodeB (genToBin h) x (transformaHermanos hs x) -- transformar hermanos a su vez transforma el resto de la lista (hermanos derechos) en binario


type Nombre = String
type Edad = Integer 
type Personass = (Nombre, Edad) 
-- el type lo usamos para definir "sinonimos" de tipos. Nombre es sinonino de string, etc. 
