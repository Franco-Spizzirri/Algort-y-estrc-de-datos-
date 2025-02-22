module Pila (Stack,pop,push,top,emptyStk,stackIsEmpty) where 
    push :: a -> Stack a -> Stack a
    pop :: Stack a -> Stack a 
    top :: Stack a -> a 
    emptyStk :: Stack a 
    stackIsEmpty :: Stack a -> Bool

    data Stack a = EmptyStk | Stk a (Stack a) deriving Show
    emptyStk = EmptyStk -- pila vacia = vacia
    push x s = Stk x s 
    pop EmptyStk = error "Stack vacio" -- si esta vacia me tira este error. 
    pop (Stk _ s) = s -- quiero eliminar el x y quedarme con el s. Me devuelve s y que me borre el primer elemento. 
    top EmptyStk = error "Stack Vacio"
    top (Stk x _) = x -- aca si me quiero quedar con x xq quiero q me muestre el elemento superior de la pila. Entonces le pido q me de el x, la s no me importa. 
    stackIsEmpty EmptyStk = True
    stackIsEmpty _ = False -- con el _ indico que el stack si tiene algo.  

-- ejemplo:
-- *Pila> s1 = push 5 emptyStk
-- *Pila> s2= push 6 s1
-- *Pila> show s2
-- "Stk 6 (Stk 5 EmptyStk)"
-- *Pila> s3= push 7 s2
-- *Pila> show s3
-- "Stk 7 (Stk 6 (Stk 5 EmptyStk))"
-- *Pila> pop (Stk 6 (Stk 5 emptyStk))
-- Stk 5 EmptyStk
-- *Pila> top (Stk 6 (Stk 5 emptyStk))
-- 6
-- *Pila> stackIsEmpty (Stk 6 (Stk 5 emptyStk))
-- False


    -- en la terminar usamos : stackIsEmpty EmptyStk, push 1 EmptyStk, push 2 (push 1 EmptyStk), stackIsEmpty (push 1 EmptyStk)

    -- Usando listas: 
    newtype Stack a = Stk [a] deriving Show 

    emptyStk = Stk [] 

    push x (Stk xs) = Stk (x:xs) -- push pone el elemento x en el tope de la lista, o sea que siempre el q entre va a quedar primero de la lista. 
    
    pop (Stk []) = error "Stack vacio"
    pop (Stk (_:xs)) = Stk xs

    top (Stk[]) = error "Stack vacio"
    top (Stk(x:_)) = x 

    stackIsEmpty (Stk[]) = True 
    stackIsEmpty (Stk _) = False 

-- ejemplo:

-- *Pila> push 2 (Stk[3])
-- Stk [2,3]
-- *Pila> push 4 (Stk [2,3])
-- Stk [4,2,3]
-- *Pila> top (Stk[4,2,3])
-- 4
-- *Pila> pop (Stk[4,2,3])
-- Stk [2,3]
-- *Pila> stackIsEmpty (Stk[2,3])
-- False
-- ambas definiciones funcionan, solo q te va a dar error xq esta definido Stack a y Stk 2 veces.  