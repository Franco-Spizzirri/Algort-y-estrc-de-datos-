-- Crear un diccionario usando list. 

module Diccionario (Dict, mkNewDict, insertDict, delDict, inDict) where

newtype Dict a = D [a] deriving Show 

mkNewDict :: Dict a
mkNewDict = D [] 

insertDict :: (Ord a) => a -> Dict a -> Dict a
insertDict x (D []) = D [x]  -- Si el diccionario está vacío, inserta el elemento.
insertDict x (D (y:ys))
    | x == y    = D (y:ys)  -- Si el elemento ya está en el diccionario, no lo insertamos.
    | x < y     = D (x:y:ys)  -- Si el elemento es menor que el primero, lo insertamos antes.
    | otherwise = D (y : ys')  -- Si el elemento es mayor, seguimos buscando recursivamente.
  where
   D ys' = insertDict x (D ys)  -- Recursión para insertar en el resto del diccionario.

delDict :: (Ord a) => a -> Dict a -> Dict a
delDict x (D []) = D []  -- Si el diccionario está vacío, retornamos el diccionario vacío. 
                        -- otra forma : delDict x (D []) = error "Diccionario vacío"  -- Si el diccionario está vacío, error. 
delDict x (D (y:ys))
    | x == y    = D ys  -- Si encontramos el elemento, lo eliminamos.
    | otherwise = D (y : ys')  -- Si no es el primero, seguimos buscando.
  where
   D ys' = delDict x (D ys)  -- Recursión para eliminar el elemento.

inDict :: (Ord a) => a -> Dict a -> Bool
inDict x (D []) = False  -- Si el diccionario está vacío, el elemento no está presente.
inDict x (D (y:ys))
    | x == y    = True  -- Si encontramos el elemento, retornamos True.
    | otherwise = inDict x (D ys)  -- Si no, seguimos buscando.



-- Diccionario usando Arbol binario: 

-- ya definido previamente el arbol binario (ojo ahi, si no hay q hacerlo primero )

newtype Dict a = Dic (ArbolBin a) deriving Show 

mkNewDict :: (Ord a) => Dict a 
mkNewDict = Dic (mkNewTree)

insertDict :: (Ord a) => a -> Dict a -> Dict a 
insertDict x (Dic t) = Dic (addTree x t)

inDict :: (Ord a) => a -> Dict a -> Bool
inDict x (Dic t) = inTree x t 

delDict :: (Ord a) => a -> Dict a -> Dict a 
delDict x (Dic t) = Dic (delTree x t)




