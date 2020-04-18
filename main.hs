{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}


module TTree where

class Dic k v d | d -> k v where
  vacio :: d
  insertar :: Ord k => k -> v -> d -> d
  buscar :: Ord k => k -> d -> Maybe v
  eliminar :: Ord k => k -> d -> d
  claves :: Ord k => d -> [k]

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v) | Leaf k v | E deriving Show

instance Ord k => Dic [k] v (TTree k v) where
  vacio = empty
  insertar = insert
  buscar = search
  eliminar = delete
  claves = keys

--empty :: devuelve un arbol vacio.
empty :: TTree k v
empty = E

--search :: devuelve el valor asociado a una clave
search :: Ord k => [k] -> TTree k v -> Maybe v
search = undefined

--insert :: grega un par (clave, valor) a un arbol. Si la clave ya está en el ́arbol, actualiza su valor
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [caracter] valor E = Leaf caracter valor
insert (caracter:clave) valor E = Node caracter Nothing E (insert clave valor E) E
insert [caracter] valor (Leaf k v) | caracter == k = Leaf k valor
                                   | caracter < k = Node k (Just v) (Leaf caracter valor) E E
                                   | caracter > k = Node k (Just v) E E (Leaf caracter valor)
insert (caracter:clave) valor (Leaf k v) | caracter == k = Node k (Just v) E (insert clave valor E) E
                                         | caracter < k = Node k (Just v) (insert clave valor E) E E
                                         | caracter > k = Node k (Just v) E E (insert clave valor E)
insert [caracter] valor (Node k v i m d) | caracter == k = Node k (Just valor) i m d
                                         | caracter < k = Node k v (insert [caracter] valor i) m d
                                         | caracter > k = Node k v i m (insert [caracter] valor d)
insert (caracter:clave) valor (Node k v i m d) | caracter == k = Node k v i (insert clave valor m) d
                                               | caracter < k = Node k v (insert (caracter:clave) valor i) m d
                                               | caracter > k = Node k v i m (insert (caracter:clave) valor d)


--delete elimina una clave y el valor asociada a ́esta en un  ́arbol.
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete = undefined

--keys :: dado un  ́arbol devuelve una lista ordenada con las claves del mismo.
keys :: TTree k v -> [[k]]
keys = undefined

