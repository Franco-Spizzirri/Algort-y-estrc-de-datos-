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

-- Definición del tipo de dato Set como una lista de elementos de tipo a
type Set a = [a]

-- emptySet: Crea un conjunto vacío
emptySet :: Set a
emptySet = []

-- setEmpty: Devuelve True si el conjunto está vacío, False si no lo está
setEmpty :: Set a -> Bool
setEmpty [] = True
setEmpty _  = False

-- inSet: Verifica si un elemento está en el conjunto (utiliza la igualdad)
inSet :: (Eq a) => a -> Set a -> Bool
inSet x set = x `elem` set

-- addSet: Añade un elemento al conjunto si no está presente
addSet :: (Eq a) => a -> Set a -> Set a
addSet x set
  | x `elem` set = set  -- Si el elemento ya está, no lo añadimos
  | otherwise = x : set  -- Si no está, lo añadimos al principio de la lista

-- delSet: Elimina un elemento del conjunto si está presente
delSet :: (Eq a) => a -> Set a -> Set a
delSet x set = filter (/= x) set  -- Filtra todos los elementos distintos a x

-- unionSet: Realiza la unión de dos conjuntos (sin duplicados)
unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet set1 set2 = foldr addSet set2 set1  -- Añade todos los elementos de set1 a set2


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