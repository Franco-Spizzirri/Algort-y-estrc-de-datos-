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

newtype Set a = St [a] deriving Show -- tengo que definir un nuevo tipo de dato implementando los metodos que me dio arriba. Por eso va el newtype. 
-- al pedirme que lo haga, pero con listas, significa que ya usando los datos previos q me dio, tengo que usarlos pero con listas. 

emptySet :: Set a
setEmpty :: Set a -> Bool
inSet :: (Eq a) => a -> Set a -> Bool
addSet :: (Eq a) => a -> Set a -> Set a
delSet :: (Eq a) => a -> Set a -> Set a
unionSet :: (Eq a) => Set a -> Set a -> Set a

emptySet = St[]

setEmpty (St []) = True
setEmpty (St _)  = False

inSet x (St []) = False
inSet x (St (y:ys)) = x == y || inSet x (St (ys))-- para chequear si el elemento esta o no en el conjunto

addSet y (St xs) = if inSet y (St (xs)) --agregara un elemento al conjunto si todavia no esta
                    then St (xs)   -- si el elemento a insertar ya esta en el conjunto, devuelve el conjunto
                    else  St (y:xs) -- si el elemento no esta en el conjunto, lo agrega


delSet x (St []) = error "Set vacio"
delSet x (St (y:ys))= if x /= y -- si el elemento a eliminar es distinto al primer elemento de la lista, puede estar en el resto de la lista
                       --then St (x : delSet y (St xs)), que ahorra el paso de llamar addSet para el primer elemento, no funciona
                       then addSet y (delSet x (St (ys)))-- si lo dicho antes sucede, llamamos recursivamente a delSet con el resto de la lista, y luego agregamos y, que es el primer elemento de la lista
                       else St ys -- si el elemento a eliminar es igual al primer elemento de la lista, devolvemos el resto de la lista 

unionSet (St []) (St (x)) = St (x) -- si el primer conjunto esta vacio y el segundo no, el rdo (resultado) es el segundo conjunto
unionSet (St (x:xs))(St(y)) = if inSet x (St y)-- si el primer elemento del primer conjunto esta en el segundo 
                                then unionSet (St (xs))(St(y)) -- entonces llamamos a la funcion con el resto del primer conjunto y el segundo
                                else unionSet (St (xs))(St(x:y))-- si el primer elemento del primer conjunto no esta en el segundo, lo une con el segundo y sigue con el resto de la lista

-- Si te pide que esten ordenados: unionSet (Set xs) (Set ys) = Set (xs ++ [y | y <- ys, not (y `elem` xs)])
-- Si no,dejalo como esta arriba. 

-- Para corroborar q todo este bien: 
-- *Set> emptySet
-- St []

-- *Set> addSet 3 emptySet
-- St [3]

-- *Set> addSet 3 (Set [4,5,6])
-- St [3,4,5,6]

-- *Set> addSet 5 (St [4,5,6])
-- St [4,5,6]  -- No se repite el 5

-- *Set> delSet 4 (St [4,5,6])
-- St [5,6]

-- *Set> unionSet (St [1,2,3]) (St [3,4,5])
-- Deberia : St [1,2,3,4,5]


--FUNCION SET DE IVAN:  

-- module Set (Set, emptySet, setIsEmpty, inSet, addSet, delSet, unionSet) where

-- emptySet:: Set a 
-- setIsEmpty:: Set a -> Bool
-- inSet:: (Eq a) => a -> Set a -> Bool
-- addSet:: (Eq a) => a -> Set a -> Set a 
-- delSet:: (Eq a) => a -> Set a -> Set a
-- unionSet:: (Eq a) => Set a -> Set a -> Set a 

-- newtype Set a = St [a] deriving Show

-- emptySet = St []

-- setIsEmpty (St []) = True
-- setIsEmpty (St _) = False

-- inSet y (St []) = False
-- inSet y (St (x:xs)) = x == y || inSet y (St (xs)) -- para chequear si el elemento esta o no en el conjunto

-- addSet y (St xs) = if inSet y (St xs) --agregara un elemento al conjunto si todavia no esta
--                    then St (xs) -- si el elemento a insertar ya esta en el conjunto, devuelve el conjunto
--                    else addSet y (St xs) -- si el elemento no esta en el conjunto, lo agrega

-- delSet y (St []) = error "Set vacio"
-- delSet y (St (x:xs)) = if y /= x -- si el elemento a eliminar es distinto al primer elemento de la lista, puede estar en el resto de la lista
--                        --then St (x : delSet y (St xs)), que ahorra el paso de llamar addSet para el primer elemento, no funciona
--                        then addSet x (delSet y (St xs)) -- si lo dicho antes sucede, llamamos recursivamente a delSet y con el resto de la lista, y luego agregamos x, que es el primer elemento de la lista
--                        else St xs -- si el elemento a eliminar es igual al primer elemento de la lista, devolvemos el resto de la lista 

-- unionSet (St []) (St (x)) = St (x) -- si el primer conjunto esta vacio y el segundo no, el rdo es es el segundo conjunto
-- unionSet (St (x:xs)) (St (y)) = if inSet x (St y) -- si el primer elemento del primer conjunto esta en el segundo 
--                                 then unionSet (St (xs)) (St (y)) -- entonces llamamos a la funcion con el resto del primer conjunto y el segundo
--                                 else unionSet (St (xs)) (St (x:y)) -- si el primer elemento del primer conjunto no esta en el segundo, lo une con el segundo y sigue con el resto de la lista
