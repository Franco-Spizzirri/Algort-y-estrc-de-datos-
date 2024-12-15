--1 Un numero perfecto es aquel que es igual a la suma de sus divisores menores que el. (Ej 6: 3,2,1). 
--Utilizando listas por comprension escribir la funcion "perfectosn" que de como resultado la lista de numeros perfectos comprendidos en el intervalo [1,n]. 

sumaDivisores :: Int -> Int 
sumaDivisores n = sum [x | x <- [1..n-1], n `mod` x == 0]

perfectosn :: Int -> [Int]
perfectosn p = [x | x <- [1..p], sumaDivisores x == x]
--              aca es para cada x en el intervalo 1..p verifica si la suma de sus divisores propios es igual a x

--2 Escribir una funcion que recibe como argumento dos listas ordenadas, y que devuelve una lista ordenada fusion de las listas de argumentos (NO se debe usar ningun metodo de clasificacion)
--juntar :: (Ord a) => [a] -> [a] -> [a] 

juntar :: (Ord a) => [a] -> [a] -> [a] 
juntar xs [] = xs -- cuando la segunda es vacia devuelvo la primera lista. 
juntar [] ys = ys -- cuando la primera es vacia devuelvo la segunda lista.
juntar (x:xs) (y:ys)
    | x <= y = x : juntar xs (y:ys) --si x es menor o igual q y, colocalo primero
    | otherwise = y : juntar ys (x:xs) -- si y es menor que x, colalo primero

--3 Escriba una funcion Qsort :: (Ord a) => [a] -> [a]. Sin usar listas por comprension. 
-- Escriba una funcion particion que reciba como argumento un valor de referencia, o pivot, y una lista de valores del mismo tipo que el pivot. Esta funcion da como resultado
-- una tupla de 2 listas de modo que en la primer lista estan todos los valores de la lista original que son menores o iguales que el pivot y la segunda lista todos los mayores
-- que el pivot. 
-- particion :: (Ord a) => a -> [a] -> ([a],[a])

particion :: (Ord a) => a -> [a] -> ([a],[a])
particion p [] = ([],[]) -- si vos estas poniendo un valor pivot pero la lista esta vacia, lo q devuelve esto es una tupla de 2 listas. Entonces si la lista esta vacia, no importa el valor del pivot te va a devolver una tupla de dos listas vacias. 
particion p (x:xs) 
    | x <= p = (x : menores, mayores) -- si x es menor al pivot los : ponen a x en la lista de los menores
    |otherwise = (menores,x : mayores) -- si es mayor pasa a los menores pone la , y usa el : para ponerlo en la lista de los mayores
    where 
        (menores,mayores) = particion p xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort menores ++ [x] ++ qsort mayores 
    where 
        (menores, mayores) = particion x xs 


--4 Escribir una version personal de la funcion zip, llamada miZip
miZip :: [a] -> [b] -> [(a,b)]
miZip _ [] = [] -- cuando la segunda lista es vacia, es vacia la tupla. 
miZip [] _ = [] 
miZip (x:xs) (y:ys) = (x,y) : miZip xs ys -- tomo los primeros elementos de las listas (el x y el y) y los emparejo, despues llamo recursivamente a mizip para q haga lo mismo con el resto de los elementos de ambas listas

--5 Usando miZip y listas por comprension escriba una funcion que realice el producto escalar de dos listas. Suma de los productos uno a uno, componente a componente de la lista.
-- Si una lista tuviera mas elementos que la otra, al agotarse uno de los operandos se detiene la suma. 
-- a1*b1 + a2*b2 + ai*bi
productoEsc :: [Int] -> [Int] -> Int
productoEsc [] _ = 0  -- Si una lista está vacía, el producto escalar es 0
productoEsc _ [] = 0  -- Lo mismo si la otra lista está vacía
productoEsc xs ys = sum [x*y | (x,y) <- miZip xs ys] --combina las dos listas xs y ys en una lista de pares (x, y) donde x es un elemento de la lista xs y y es el elemento correspondiente en la lista ys. 
-- para cada par (x, y) generado por miZip xs ys, se calcula el producto x * y. Sum devuelve la suma de esos productos. 

--5 Usando solo la funcion miZip escriba la funcion indexado. Dada una lista produce una lista de pares donde cada elemento de la lista tiene su posicion dentro de la misma. 
--La indezacion comienza en 1. 
--Ej: indexado ["Juan", "Pedro", "Luis"] => [("Juan",1),("Pedro",2),("Luis", 3)] 

indexado :: [a] -> [(a,Int)]
indexado xs = miZip xs [1..]  -- Usa miZip para combinar la lista xs con la lista de índices empezando en 1

--6 Escribir una funcion que inserta elementos en una lista de manera de mantenerla ordenada de menor a mayor. De esta forma cada operacion Head sobre la lista devuelve el
--elemento mas chico almacenado en ella 

inserta2 :: (Ord a) => a -> [a] -> [a]
inserta2 x [] = [x]
inserta2 x (y:ys) 
    | x <= y = x : (y:ys) -- Si el nuevo elemento es menor o igual que y, lo insertamos antes de y. 
    |otherwise = y : inserta2 x ys  -- Si el nuevo elemento es mayor que y, seguimos buscando en la cola. Hasta encontrar un x y colocarlo adelante de esa y. 

--EJemplo: 

--insertar 4 [1, 3, 5, 7]
-- Comienza con x = 4, y = 1:
-- 4 > 1, por lo que seguimos a la cola con [3, 5, 7].
-- Comienza con x = 4, y = 3:
-- 4 > 3, por lo que seguimos a la cola con [5, 7].
-- Comienza con x = 4, y = 5:
-- 4 <= 5, por lo que insertamos 4 antes de 5.
-- Resultado final: [1, 3, 4, 5, 7]

-- 7 Considerando la siguiente funcion: 
-- split :: (Ord a) => a -> [a] -> ([a], [a])
-- split x l = ([y |y <-l , y <=x], [y | y <- l , y > x])
-- El primer corchet me dice que toma los y de la lista l y lo q esta luego de la coma es cuales tipos de y toma (q en este caso son los menosres o iguales a x)
-- en el segundo toma los y de la lista l que son mayores a x. 
-- Defina una version de esta funcion que trabaje en exactamente una sola pasada a la lista "l". 
split :: (Ord a) => a -> [a] -> ([a], [a])
split _ [] = ([], [])
split x (y:ys)
    | y <= x = (y : izq , der)  -- Si y <= x, lo agregamos a la sublista izquierda. Va asi xq arriba te pide los Y de la lista que son menores a x, no tiene sentido poner x xq no es un elemento propio de la lista, Y si. 
    |otherwise = (izq , y : der) -- Si y > x, lo agregamos a la sublista derecha. Agarramos los y que son mayores a x
    where
        (izq , der) = split x ys -- Recursión: aplicamos split al resto de la lista
    
-- El resultado de la recursión se obtiene directamente a través de la expresión where, que evalúa split x ys para obtener las dos sublistas (left y right) de la cola ys de la lista.
-- El uso de where permite definir las sublistas left y right de forma más clara y sin necesidad de let.

-- 8 Defina un tipo de dato Arbol Binario de busqueda (ArbolBin)
-- Escriba el metodo addTree e inOrderTree
--addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a 
-- inOrderTree :: (Ord a) => ArbolBin a -> [a]  (insertas el arbol y te devuelve la lista de los nodos en inorder). 
-- El listado en orden de arbol se define de la siguiente manera: primero se lista el arbol izq, luego la raiz y finalmente se lista en orden el arbol derecho. 


data ArbolBin a = EmptyBT | NodoBt a (ArbolBin a) (ArbolBin a) deriving Show

--mkNewTree :: (Ord a) => ArbolBin a
--inTree :: (Ord a) => a -> ArbolBin a -> Bool
addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a 
--delTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a 
inOrderTree :: (Ord a) => ArbolBin a -> [a]

addTree x EmptyBT = NodoBt x EmptyBT EmptyBT -- aca cuando es vacio aclaro que es de tipo NodoBt y q tiene una raiz x con dos nodos vacios
addTree x (NodoBt y lf rt)
    | x == y = NodoBt y lf rt -- y es el valor del nodo actual. 
    | x < y = NodoBt y (addTree x lf) rt -- La recursión se encarga de seguir buscando la posición adecuada para insertar x en el subárbol izquierdo de y.
    | x > y = NodoBt y lf (addTree x rt)

inOrderTree EmptyBT = [] -- si el arbol esta vacio devuelvo lista vacia. 
inOrderTree (NodoBt x lf rt) = inOrderTree lf ++ [x] ++ inOrderTree rt -- Es por eso que en la función inOrderTree, cada vez que encontramos un nodo, 

preOrderTree :: (Ord a) => ArbolBin a -> [a]
preOrderTree EmptyBT = []
preOrderTree (NodoBt x lf rt) = [x] ++ preOrderTree lf ++ preOrderTree rt

postOrderTree :: (Ord a) => ArbolBin a -> [a]
postOrderTree EmptyBT = []
postOrderTree (NodoBt x lf rt) = postOrderTree lf ++ postOrderTree rt ++ [x]

--llamamos de forma recursiva a la función sobre su subárbol izquierdo, luego procesamos el nodo actual (lo agregamos a la lista), y después llamamos recursivamente a la función sobre su subárbol derecho.
--Main> addTree 4  EmptyBT
--NodoBt 4 EmptyBT EmptyBT
-- *Main> addTree 5 (NodoBt 4 EmptyBT EmptyBT)
-- NodoBt 4 EmptyBT (NodoBt 5 EmptyBT EmptyBT)
-- *Main> addTree 6 (NodoBt 4 EmptyBT EmptyBT)
-- NodoBt 4 EmptyBT (NodoBt 6 EmptyBT EmptyBT)
-- *Main> addTree 7 (NodoBt 4 EmptyBT (NodoBt 6 EmptyBT EmptyBT))
-- NodoBt 4 EmptyBT (NodoBt 6 EmptyBT (NodoBt 7 EmptyBT EmptyBT))
-- *Main> addTree 3 (NodoBt 4 EmptyBT (NodoBt 6 EmptyBT (NodoBt 7 EmptyBT EmptyBT)))
-- NodoBt 4 (NodoBt 3 EmptyBT EmptyBT) (NodoBt 6 EmptyBT (NodoBt 7 EmptyBT EmptyBT))
-- *Main> addTree 2 (NodoBt 4 (NodoBt 3 EmptyBT EmptyBT) (NodoBt 6 EmptyBT (NodoBt 7 EmptyBT EmptyBT)))         
-- NodoBt 4 (NodoBt 3 (NodoBt 2 EmptyBT EmptyBT) EmptyBT) (NodoBt 6 EmptyBT (NodoBt 7 EmptyBT EmptyBT))
-- *Main> inOrderTree (NodoBt 4 (NodoBt 3 (NodoBt 2 EmptyBT EmptyBT) EmptyBT) (NodoBt 6 EmptyBT (NodoBt 7 EmptyBT EmptyBT)))
-- [2,3,4,6,7]


-- Escribir el modulo diccionario con una implementacion de los mismos usando List. 



