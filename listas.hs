{-
  modulo que implementa funciones recursivas sobre listas
-}

module Listas where

--Une dos listas de enteros dadas y retorna un unica lista ordenada
unir_y_ordenar::[Int]->[Int]->[Int]
unir_y_ordenar [] lista = ordenar lista
unir_y_ordenar (x:xs) lista = unir_y_ordenar xs (x:lista)

--Inserta un elemento en una lista antes del primer elemento mayor encontrado
insertar::Int->[Int]->[Int]
insertar a [] = [a]
insertar a (x:xs) = if a<x then  a:x:xs else x:(insertar a xs)

--Ordena una lista de enteros
ordenar::[Int]->[Int]
ordenar [] = []
ordenar (x:xs) = insertar x (ordenar xs)

--Forma tuplas de elementos (x,y) tales que x esta en la primera lista e y en la segunda
combinar::[Int]->[Int]->[(Int,Int)]
combinar [] lista = []
combinar (x:xs) (y:ys) = (combinar_simple x (y:ys))++(combinar xs (y:ys))

--Forma tuplas combinando un elemento entero con cada elemento de una lista
combinar_simple::Int->[Int]->[(Int,Int)]
combinar_simple a [] = []
combinar_simple a (y:ys) = (a,y):(combinar_simple a ys)