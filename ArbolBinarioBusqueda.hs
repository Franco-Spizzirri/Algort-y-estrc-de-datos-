data ArbolBin a = VacioBin | NodoBin a (ArbolBin a) (ArbolBin a) deriving Show 

mkNewTree :: (Ord a) => ArbolBin a 
inTree :: (Ord a) => a -> ArbolBin a -> Bool 
delTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a 
addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a 
minTree :: (Ord a) => ArbolBin a -> (a, ArbolBin a)

mkNewTree = VacioBin 

inTree x VacioBin = False
inTree x (NodoBin y lf rt)
    | x == y = True 
    | x < y = inTree x lf
    | x > y = inTree x rt 

addTree x VacioBin = NodoBin x VacioBin VacioBin
addTree x (NodoBin y lf rt)
    | x == y = NodoBin y lf rt --es el mismo elemento, no se pueden agregar dos veces el mismo. Por eso retorna todo igual. 
    | x < y = NodoBin y (addTree x lf) rt 
    | x > y = NodoBin y lf (addTree x rt) 

-- para eliminar hay que hacer primero la funcion de encontrar el valor minimo para luego reemplazar x si eliminamos una raiz y quedan ambos hijos sueltos. 

minTree (NodoBin x VacioBin rt) = (x, rt)
minTree (NodoBin x lf rt) = let (y, new_lf) = minTree lf 
                                in (y, NodoBin x new_lf  rt)

-- ahora si metemos para borrar: 

deltree x VacioBin = VacioBin
delTree x (NodoBin y lf VacioBin)
    | x == y = lf
delTree x (NodoBin y VacioBin rt )
    | x == y = rt 
delTree x (NodoBin y lf rt)
    | x < y = NodoBin y (delTree x lf) rt 
    | x > y = NodoBin y lf (delTree x rt)
    | x == y = let (k, wt) = minTree (rt)
                in (NodoBin k lf wt)

-- ejemplo:
-- *Main> mkNewTree
-- VacioBin
-- *Main> addTree 2 VacioBin
-- NodoBin 2 VacioBin VacioBin
-- *Main> addTree 10 VacioBin
-- NodoBin 10 VacioBin VacioBin
-- *Main> addTree 2 (NodoBin 10 VacioBin VacioBin)
-- NodoBin 10 (NodoBin 2 VacioBin VacioBin) VacioBin
-- *Main> addTree 12 (NodoBin 10 (NodoBin 2 VacioBin VacioBin) VacioBin)
-- NodoBin 10 (NodoBin 2 VacioBin VacioBin) (NodoBin 12 VacioBin VacioBin)
-- *Main> inTree 12 (NodoBin 10 (NodoBin 2 VacioBin VacioBin) (NodoBin 12 VacioBin VacioBin))
-- True
-- *Main> inTree 4 (NodoBin 10 (NodoBin 2 VacioBin VacioBin) (NodoBin 12 VacioBin VacioBin)) 
-- False
-- *Main> delTree 12 (NodoBin 10 (NodoBin 2 VacioBin VacioBin) (NodoBin 12 VacioBin VacioBin))
-- NodoBin 10 (NodoBin 2 VacioBin VacioBin) VacioBin
-- *Main>

