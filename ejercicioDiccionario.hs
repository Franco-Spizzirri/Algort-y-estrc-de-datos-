module Diccionario (Dict, mkNewDict, insertDict, delDict, inDict) where

-- Definición del tipo Dict
newtype Dict a = D [a] deriving Show 

-- Función para crear un diccionario vacío
mkNewDict :: Dict a
mkNewDict = D []  -- Retorna un diccionario vacío (lista vacía).

-- Función para insertar un elemento en el diccionario
insertDict :: (Ord a) => a -> Dict a -> Dict a
insertDict x (D []) = D [x]  -- Si el diccionario está vacío, inserta el elemento.
insertDict x (D (y:ys))
    | x == y    = D (y:ys)  -- Si el elemento ya está en el diccionario, no lo insertamos.
    | x < y     = D (x:y:ys)  -- Si el elemento es menor que el primero, lo insertamos antes.
    | otherwise = D (y : ys')  -- Si el elemento es mayor, seguimos buscando recursivamente.
  where
   D ys' = insertDict x (D ys)  -- Recursión para insertar en el resto del diccionario.

-- Función para eliminar un elemento del diccionario
delDict :: (Ord a) => a -> Dict a -> Dict a
delDict x (D []) = D []  -- Si el diccionario está vacío, retornamos el diccionario vacío. 
                        -- otra forma : delDict x (D []) = error "Diccionario vacío"  -- Si el diccionario está vacío, error. 
delDict x (D (y:ys))
    | x == y    = D ys  -- Si encontramos el elemento, lo eliminamos.
    | otherwise = D (y : ys')  -- Si no es el primero, seguimos buscando.
  where
   D ys' = delDict x (D ys)  -- Recursión para eliminar el elemento.

-- Función para saber si un elemento está en el diccionario
inDict :: (Ord a) => a -> Dict a -> Bool
inDict x (D []) = False  -- Si el diccionario está vacío, el elemento no está presente.
inDict x (D (y:ys))
    | x == y    = True  -- Si encontramos el elemento, retornamos True.
    | otherwise = inDict x (D ys)  -- Si no, seguimos buscando.
