-- Un CONJUNTO o SET, es una colección de ítems del mismo tipo distingibles entre si por su clave o  valor, en la cual un ítem puede ser testeado  si es miembro, insertado o  borrado de la colección. La cantidad  de elementos distintos es lo que se denomina el tamaño del conjunto.
-- Module Set (Set,  emptySet, setEmpty, inSet, addSet, delSet) where
-- emptySet  ::  Set a
-- setEmpty  ::  Set a  -> Bool
-- inSet      :: (Eq a) => a -> Set a -> Bool
-- addSet    :: (Eq a) => a -> Set a -> Set a
-- delSet    :: (Eq a) => a -> Set a -> Set a
-- unionSet :: (Eq a) => Set a -> Set a -> Set a
-- Defina el tipo de dato e implemente los métodos del nuevo tipo de dato, utilizando  listas no ordenadas y sin duplicados.El metodo unionSet (Union de dos conjuntos) se escribira haciendo uso de los metodos ya definidos, es decir no se operará directamente la lista sino se operará al SET.

module Set (Set, emptySet, setEmpty, inSet, addSet, delSet, unionSet) where


type Set a = [a]  


emptySet :: Set a
emptySet = []

setEmpty :: Set a -> Bool
setEmpty [] = True
setEmpty _  = False

inSet :: (Eq a) => a -> Set a -> Bool
inSet _ [] = False
inSet x (y:ys)
  | x == y    = True
  | otherwise = inSet x ys

addSet :: (Eq a) => a -> Set a -> Set a
addSet x [] = [x] 
addSet x (y:ys)
  | x == y    = y : ys  
  | otherwise = y : addSet x ys  

delSet :: (Eq a) => a -> Set a -> Set a
delSet _ [] = []
delSet x (y:ys)
  | x == y    = ys  
  | otherwise = y : delSet x ys  

unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet [] set2 = set2 
unionSet (x:xs) set2
  | inSet x set2 = unionSet xs set2  
  | otherwise = x : unionSet xs set2  
