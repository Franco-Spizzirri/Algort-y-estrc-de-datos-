-- a) Escribir una función que inserta elementos en una lista de manera de mantenerla ordenada de menor a mayor. De esta forma cada operación Head sobre la lista devuelve el elemento más chico almacenado en ella.
-- Inserta:: (Ord a)=> a->[a]->[a]

inserta:: (Ord a)=> a->[a]->[a]
inserta x [] = [x]
inserta x (y:ys)
    | x <= y = x : (y:ys)
    | otherwise = y : inserta x ys 

-- b) Escriba una función Qsort::(Ord a)=>[a]->[a]. Sin utilizar listas por comprensión.

-- Observacion: Escriba una funcion particion que reciba como argumento, un valor de referencia o pivot y a una lista de valores del mismo tipo que el pivot. Esta funcion da como resultado una tupla con dos listas ( l1 , l2) ... de modo  que en l1 estan todos los valores de la lista original que son menores o iguales que el pivot y  en l2 todos los mayores que el pivot.

-- particion :: Ord a => a->[a]->([a],[a])

particion :: Ord a => a->[a]->([a],[a])
particion p [] = ([],[])
particion p (x:xs)
    | x <= p = (x:menores, mayores)
    |otherwise = (menores,x:mayores)
    where
        (menores,mayores) = particion p xs

qsort::(Ord a)=>[a]->[a]
qsort [] = []
qsort (x:xs) = qsort menores ++ [x] ++ qsort mayores 
    where 
        (menores, mayores) = particion x xs 


-- Recordemos que la función  de biblioteca ZIP, recibe como argumento dos listas (x:xs) e (y:ys)  y produce una lista de tuplas (i,j) donde los i provienen  de la primera lista y los j de la segunda. Cuando una lista es mas larga que la otra, el resultado contempla solo los pares hasta donde pudieron formarse….

-- Ej   zip [1,2,3] [10..] = [(1,10), (2,11), (3,12)] 

-- a)Escriba una versión personal de la función zip, llamada miZip

-- miZip :: [a]->[b]->[(a,b)]

miZip :: [a]->[b]->[(a,b)]
miZip [] _ = []
miZip _ [] = []
miZip (x:xs) (y:ys) = (x,y) : miZip xs ys 

-- b) Utilizando miZIP y listas por comprension. Escriba una función que realice el producto escalar de dos listas. Donde producto escalar estaría definido como la suma de los productos uno a uno, componente a componente de cada lista. Si una lista tuviera más elementos que la otra, al agotarse uno de los operandos se detiene la suma.

--   a1*b1 + a2*b2+…+ai*bi+…+ an*bn.

productoEscalar :: [Int] -> [Int] -> Int
productoEscalar [] _ = 0
productoEscalar _ [] = 0
productoEscalar (x:xs) (y:ys) = sum [x*y | (x,y) <- miZip xs ys]

