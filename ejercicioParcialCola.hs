-- 9 Una cola de prioridad es una estructura de datos que almacena elementos clasificables. Con la particularidad que, cuando se saca uno de ella siempre se extrae el elemento
-- con menor clave, de ahi su nombre pues clasifica los elementos en funcion de su prioridad. La prioridad mas baja primero. 
-- mkqrp: Instancia una nueva cola de prioridad vacia. 
-- addqpr: Agrega un nuevo elemento a la cola de prioridad. 
-- nextqpr: Devuelve el elemento con clave mas baja de la cola de prioridad. 
-- popqpr: devuelve una cola de prioridad donde se ha quitado el nextqpr. 
-- Defina el TAD ColaPrioridad, e implemente el mismo utilizando un arbol binario de busqueda como estructura de almacenamiento. 
-- Escribir todas las funciones necesarias para la manipulacion de la estructura subyascente, es decir para manipular el arbol. 
-- Recordar como extraer el elemento con clave mas pequeña de un arbol. 

module ColaPrioridad (ColaPrioridad, mkQpr, addQpr, nextQpr, popQpr) 
where
data BinArbol a = EmptyBt | NodoBT a (BinArbol a) (BinArbol a) deriving Show -- si voy a usar un arbol binario para el almacenamiento tambien tengo que definirlo. 
data ColaPrioridad a = Cola { arbol2 :: BinArbol a } deriving Show --El operador :: se utiliza para especificar el tipo de un campo. En este caso, Arbol2 :: BinArbol a significa que el campo Arbol2 es de tipo BinArbol a, es decir, es un árbol binario de búsqueda cuyos nodos contienen valores de tipo a.

mkQpr :: ColaPrioridad a 
mkQpr = Cola EmptyBt          -- misma caracteristica que cuando definis un arbol binario vacio. 

addQpr :: (Ord a) => a -> ColaPrioridad a -> ColaPrioridad a
addQpr x (Cola t) = Cola (addTree2 x t)
  where 
    addTree2 :: (Ord a) => a -> BinArbol a -> BinArbol a
    addTree2 x EmptyBt = NodoBT x EmptyBt EmptyBt
    addTree2 x (NodoBT y izq der)
      | x < y     = NodoBT y (addTree2 x izq) der  -- Coloca en el subárbol izquierdo los valores menores.
      | otherwise = NodoBT y izq (addTree2 x der)  -- Coloca en el subárbol derecho los valores mayores.


nextQpr :: (Ord a) => ColaPrioridad a -> Maybe a
nextQpr (Cola EmptyBt) = Nothing  -- Si el árbol está vacío, no hay valor mínimo.
nextQpr (Cola t) = Just (obtenerMinimo t)  -- Llamamos a la función auxiliar para obtener el mínimo.
  where
    obtenerMinimo :: (Ord a) => BinArbol a -> a
    obtenerMinimo (NodoBT v EmptyBt _) = v  -- Si llegamos al nodo más a la izquierda, ese es el valor mínimo.
    obtenerMinimo (NodoBT _ izq _) = obtenerMinimo izq  -- Si no, seguimos hacia la izquierda.

popQpr (Cola EmptyBt) = Cola EmptyBt
popQpr (Cola t) = Cola (delTree2 t)
  where
    delTree2 :: (Ord a) => BinArbol a -> BinArbol a
    delTree2 EmptyBt = EmptyBt
    delTree2 (NodoBT _ EmptyBt der) = der
    delTree2 (NodoBT x izq der) = NodoBT x (delTree2 izq) der






