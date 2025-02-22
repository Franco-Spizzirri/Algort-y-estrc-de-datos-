-- Cola normal: 

module Cola (Queue, emptyQueue, inQueue, delQueue, isQueueEmpty, front) where 

emptyQueue :: Queue a 
inQueue :: a -> Queue a -> Queue a 
delQueue :: Queue a -> Queue a 
isQueueEmpty:: Queue a -> Bool -- pregunto si la cola en general esta vacia
front :: Queue a -> a 

-- ahora lo implementamos usando listas []
newtype Queue a = C [a] deriving Show 

emptyQueue = C []

inQueue x (C s) = C (s ++ [x])

delQueue (C []) = error "Cola vacia"
delQueue (C (x:xs)) = C xs 

front (C []) = error "Cola vacia"
front (C (x:xs)) = x 

isQueueEmpty (C []) = True
isQueueEmpty _ = False 

-- ejemplo de uso: 
-- *Cola> s1 = C [2,3,4]
-- *Cola> delQueue s1
-- C [3,4]
-- *Cola> front s1
-- 2
-- *Cola> isQueueEmpty s1
-- False
-- *Cola> inQueue 5 s1
-- C [2,3,4,5]


--Cola comun 
data Queue a = EmptyQ | Q a (Queue a) deriving Show

emptyQueue = EmptyQ

enQueue x EmptyQ = Q x EmptyQ -- agregar elemento a una cola vacia
enQueue x (Q y ys) = Q y (enQueue x ys) -- agregar un elemento a una cola que tiene mas de un elemento.


deQueue EmptyQ = error "Cola vacia"
deQueue (Q _ s) = s

front EmptyQ = error "Cola Vacia"
front (Q x _) = x

queueIsEmpty EmptyQ = True
queueIsEmpty _ = False

-- esto comentalo por que si no, no funciona. 

-- 9 Una cola de prioridad es una estructura de datos que almacena elementos clasificables. Con la particularidad que, cuando se saca uno de ella siempre se extrae el elemento
-- con menor clave, de ahi su nombre pues clasifica los elementos en funcion de su prioridad. La prioridad mas baja primero. 
-- mkqrp: Instancia una nueva cola de prioridad vacia. 
-- addqpr: Agrega un nuevo elemento a la cola de prioridad. 
-- nextqpr: Devuelve el elemento con clave mas baja de la cola de prioridad. 
-- popqpr: devuelve una cola de prioridad donde se ha quitado el nextqpr. 
-- Defina el TAD ColaPrioridad, e implemente el mismo utilizando un arbol binario de busqueda como estructura de almacenamiento. 
-- Escribir todas las funciones necesarias para la manipulacion de la estructura subyascente, es decir para manipular el arbol. 
-- Recordar como extraer el elemento con clave mas pequeña de un arbol. 


data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) deriving Show

nuevoArbol:: (Ord a) => Arbol a 
minimo:: (Ord a) => Arbol a -> a 
agregar:: (Ord a) => a -> Arbol a -> Arbol a 
eliminar:: (Ord a) => Arbol a -> Arbol a 

nuevoArbol = Vacio 

agregar x Vacio = Nodo x Vacio Vacio 
agregar x (Nodo y izq der) 
 | x == y = Nodo y izq der 
 | x < y = Nodo y (agregar x izq) der 
 | x > y = Nodo y izq (agregar x der) 
 
minimo (Nodo y Vacio der) = y 
minimo (Nodo y izq Vacio) = minimo izq 
minimo (Nodo y izq der) = minimo izq 

eliminar Vacio = Vacio 
eliminar (Nodo y Vacio der) = der 
eliminar (Nodo y izq der) = Nodo y (eliminar izq) der 

newtype ColaPrioridad a = Cp (Arbol a) deriving Show

mkQpr:: (Ord a) => ColaPrioridad a 
addQpr:: (Ord a) => a -> ColaPrioridad a -> ColaPrioridad a
nextQpr:: (Ord a) => ColaPrioridad a -> a 
popQpr:: (Ord a) => ColaPrioridad a -> ColaPrioridad a 

mkQpr = Cp (nuevoArbol) 

addQpr x (Cp s) = Cp (agregar x s) 

nextQpr (Cp s) = minimo s 

popQpr (Cp s) = Cp (eliminar s)


