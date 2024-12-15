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

    -- en la terminar usamos : stackIsEmpty EmptyStk, push 1 EmptyStk, push 2 (push 1 EmptyStk), stackIsEmpty (push 1 EmptyStk)