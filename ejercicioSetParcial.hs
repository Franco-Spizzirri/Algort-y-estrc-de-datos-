--10 Un conjunto o set es una coleccion de items del mismo tipo distinguibles entre si por su clave o valor, en la cual un item puede ser testeado
--si es miembro, insertado o borrado de la coleccion. La cantidad de elementos distintos es lo que se denomina el tamaño del conjunto. 

-- module Set(Set, emptySet, setEmpty, inSet, addSet, delSet) where
    --emptySet :: Set a
    --setEmpty :: Set a -> Bool
    -- inSet :: (Eq a) => a -> Set a -> Bool
    -- addSet :: (Eq a) => a -> Set a -> Set a
    -- delSet :: (Eq a) => a -> Set a -> Set a
    --unionSet :: (Eq a) => Set a -> Set a -> Set a

-- Defina el tipo de dato e implemente los metodos del nuevo tipo de dato, utilizando listas no ordenadas y sin duplicados. El metodo unionSet (Union de dos conjuntos)
--se escribira haciendo uso de los metodos ya definidos, es decir no se operara directamenta la lista sino se operara el Set. 

module Set (Set, emptySet, setEmpty, inSet, addSet, delSet, unionSet) where

type Set a = [a]

emptySet :: Set a
emptySet = []

setEmpty :: Set a -> Bool
setEmpty [] = True
setEmpty _  = False

inSet :: (Eq a) => a -> Set a -> Bool
inSet x set = x `elem` set

addSet :: (Eq a) => a -> Set a -> Set a
addSet x set
  | x `elem` set = set  -- Si el elemento ya está, no lo añadimos
  | otherwise = x : set  -- Si no está, lo añadimos al principio de la lista

delSet :: (Eq a) => a -> Set a -> Set a
delSet _ [] = []
delSet x (y:ys)
  | x == y = delSet x ys 
  | otherwise = y : delSet x ys 

unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet [] set2 = set2  -- Caso base: conjunto vacío, la unión es el segundo conjunto
unionSet (x:xs) set2 
  | x `elem` set2 = unionSet xs set2 --Si 'x' ya está en 'set2', lo omitimos
  | otherwise = x : unionSet xs set2 -- Si no está, lo añadimos a 'set2'


-- *Set> inSet 1 s1
-- True
-- *Set> inSet 4 s1
-- False
-- *Set> inSet 4 s2
-- True
-- *Set> inSet 1 s2
-- False
-- *Set> s3 = addSet 5 s1
-- *Set> inSet 1 s3      
-- True
-- *Set> inSet 5 s3
-- True
-- *Set> s4 = delSet 3 s2
-- *Set> inSet 4 s2      
-- True
-- *Set> inSet 3 s2
-- True
-- *Set> inSet 3 s4
-- False
-- *Set> s5 = unionSet s1 s2
-- *Set> show (s5)
-- "[1,2,3,4]"
-- *Set> show (s4)
-- "[4]"
-- *Set> show (s3)
-- "[5,1,2]"
-- *Set>
