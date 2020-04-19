{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module TTree where

import qualified Data.Tree

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
search clave E = Nothing
search [caracter] (Leaf k v) = if caracter == k then Just v else Nothing
search (caracter:clave) (Leaf k v) = Nothing
search [caracter] (Node k v i m d) | caracter == k = v
                                   | caracter < k = search [caracter] i
                                   | caracter > k = search [caracter] d
search (caracter:clave) (Node k v i m d) | caracter == k = search clave m
                                         | caracter < k = search (caracter:clave) i
                                         | caracter > k = search (caracter:clave) d

--insert :: agrega un par (clave, valor) a un arbol. Si la clave ya está en el ́arbol, actualiza su valor
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [caracter] valor E = Leaf caracter valor
insert (caracter:clave) valor E = Node caracter Nothing E (insert clave valor E) E
insert [caracter] valor (Leaf k v) | caracter == k = Leaf k valor
                                   | caracter < k = Node k (Just v) (Leaf caracter valor) E E
                                   | caracter > k = Node k (Just v) E E (Leaf caracter valor)
insert (caracter:clave) valor (Leaf k v) | caracter == k = Node k (Just v) E (insert clave valor E) E
                                         | caracter < k = Node k (Just v) (insert (caracter:clave) valor E) E E
                                         | caracter > k = Node k (Just v) E E (insert (caracter:clave) valor E)
insert [caracter] valor (Node k v i m d) | caracter == k = Node k (Just valor) i m d
                                         | caracter < k = Node k v (insert [caracter] valor i) m d
                                         | caracter > k = Node k v i m (insert [caracter] valor d)
insert (caracter:clave) valor (Node k v i m d) | caracter == k = Node k v i (insert clave valor m) d
                                               | caracter < k = Node k v (insert (caracter:clave) valor i) m d
                                               | caracter > k = Node k v i m (insert (caracter:clave) valor d)

-- TODO: optimizar?
--merge :: combina dos arboles xd
merge :: TTree k v -> TTree k v -> TTree k v
merge E a                = a
merge l@(Leaf k v) E     = l
merge (Leaf k v) a       = Node k (Just v) E E a
merge (Node k v i m d) a = Node k v i m (merge d a)

--delete elimina una clave y el valor asociada a ́esta en un  ́arbol.
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete clave E                 = E
delete [caracter] l@(Leaf k v) = if caracter == k then E else l
delete _ l@(Leaf k v)          = l
delete [caracter] (Node k v i m d) | caracter == k = (case m of E -> merge i d
                                                                _ -> Node k Nothing i m d)
                                   | caracter < k = Node k v (delete [caracter] i) m d
                                   | caracter > k = Node k v i m (delete [caracter] d)
delete (caracter:clave) (Node k v i m d) | caracter == k = (case v of Nothing -> (let resultado = (delete clave m) in (case resultado of E -> merge i d
                                                                                                                                         _ -> Node k v i resultado d))
                                                                      Just _ -> Node k v i (delete clave m) d)
                                         | caracter < k = Node k v (delete (caracter:clave) i) m d
                                         | caracter > k = Node k v i m (delete (caracter:clave) d)

--keys :: dado un  ́arbol devuelve una lista ordenada con las claves del mismo.
keys :: TTree k v -> [[k]]
keys E                = []
keys (Leaf k v)       = [[k]]
keys (Node k v i m d) = (keys i) ++ map (k:) (keys m) ++ (keys d) ++ (case v of Nothing -> []
                                                                                Just _ -> [[k]])

ttreeToTree :: (Show k, Show v) => TTree k v -> Data.Tree.Tree [Char]
ttreeToTree E = Data.Tree.Node "" []
ttreeToTree (Leaf k v) = Data.Tree.Node (show k ++ " " ++ show v) []
ttreeToTree (Node k Nothing i m d) = Data.Tree.Node (show k) (map ttreeToTree [i, m, d])
ttreeToTree (Node k (Just v) i m d) = Data.Tree.Node (show k ++ " " ++ show v) (map ttreeToTree [i, m, d])
