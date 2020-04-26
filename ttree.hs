{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module TTree where

import qualified Data.Tree

class Dic k v d | d -> k v where
  vacio :: d
  insertar :: Ord k => k -> v -> d -> d
  buscar :: Ord k => k -> d -> Maybe v
  eliminar :: Ord k => k -> d -> d
  claves :: Ord k => d -> [k]

-- invariantes:
-- Ningún TTree es un Node k Nothing i E d
-- Ningún TTree es un Node k v E E E
-- TODO: faltan invariantes (orden de claves por ej)
data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v) | Leaf k v | E

instance Ord k => Dic [k] v (TTree k v) where
  vacio = empty
  insertar = insert
  buscar = search
  eliminar = delete
  claves = keys

instance (Show k, Show v) => Show (TTree k v) where
  show tree = Data.Tree.drawTree (ttreeToTree tree)

-- ttreeToTree :: devuelve la representación de un TTree como un Data.Tree.Tree String
ttreeToTree :: (Show k, Show v) => TTree k v -> Data.Tree.Tree String
ttreeToTree E = Data.Tree.Node "" []
ttreeToTree (Leaf k v) = Data.Tree.Node (show k ++ " " ++ show v) []
ttreeToTree (Node k Nothing i m d) = Data.Tree.Node (show k) (map ttreeToTree [i, m, d])
ttreeToTree (Node k (Just v) i m d) = Data.Tree.Node (show k ++ " " ++ show v) (map ttreeToTree [i, m, d])

--empty :: devuelve un TTree vacio
empty :: TTree k v
empty = E

--search :: devuelve (quizás) el valor asociado a una clave en un TTree
search :: Ord k => [k] -> TTree k v -> Maybe v
search clave E = Nothing
search [caracter] (Leaf k v) = if caracter == k then Just v else Nothing
search (caracter:clave) (Leaf _ _) = Nothing
search [caracter] (Node k v i _ d) | caracter == k = v
                                   | caracter < k = search [caracter] i
                                   | caracter > k = search [caracter] d
search s@(caracter:clave) (Node k _ i m d) | caracter == k = search clave m
                                           | caracter < k = search s i
                                           | caracter > k = search s d

--insert :: agrega un par (clave, valor) a un TTree. Si la clave ya está en el TTree, actualiza su valor
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [caracter] valor E = Leaf caracter valor
insert (caracter:clave) valor E = Node caracter Nothing E (insert clave valor E) E
insert [caracter] valor (Leaf k v) | caracter == k = Leaf k valor
                                   | caracter < k = Node k (Just v) (Leaf caracter valor) E E
                                   | caracter > k = Node k (Just v) E E (Leaf caracter valor)
insert s@(caracter:clave) valor (Leaf k v) | caracter == k = Node k (Just v) E (insert clave valor E) E
                                           | caracter < k = Node k (Just v) (insert s valor E) E E
                                           | caracter > k = Node k (Just v) E E (insert s valor E)
insert [caracter] valor (Node k v i m d) | caracter == k = Node k (Just valor) i m d
                                         | caracter < k = Node k v (insert [caracter] valor i) m d
                                         | caracter > k = Node k v i m (insert [caracter] valor d)
insert s@(caracter:clave) valor (Node k v i m d) | caracter == k = Node k v i (insert clave valor m) d
                                                 | caracter < k = Node k v (insert s valor i) m d
                                                 | caracter > k = Node k v i m (insert s valor d)

-- merge :: TODO: completar
merge :: TTree k v -> TTree k v -> TTree k v
merge E d = d
merge i E = i
merge (Leaf k v) d = Node k (Just v) E E d
merge (Node k v i m d) d2 = Node k v i m (merge d d2)

-- cleanup :: TODO: completar
cleanup :: TTree k v -> TTree k v
cleanup (Node k (Just v) E E E) = Leaf k v
cleanup n@(Node k (Just v) _ _ _) = n

--delete :: elimina una clave y el valor asociada a ́esta en un TTree
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete clave E = E
delete [caracter] l@(Leaf k v) = if caracter == k then E else l
delete (caracter:clave) l@(Leaf _ _) = l
delete [caracter] (Node k v@(Just _) i E d) | caracter == k = merge i d
                                            | caracter < k = cleanup (Node k v (delete [caracter] i) E d)
                                            | caracter > k = cleanup (Node k v i E (delete [caracter] d))
delete [caracter] (Node k v i m d) | caracter == k = Node k Nothing i m d
                                   | caracter < k = Node k v (delete [caracter] i) m d
                                   | caracter > k = Node k v i m (delete [caracter] d)
delete s@(caracter:clave) (Node k Nothing i m d) | caracter == k = let resultado = delete clave m in case resultado of E -> merge i d
                                                                                                                       _ -> Node k Nothing i resultado d
                                                 | caracter < k = Node k Nothing (delete s i) m d
                                                 | caracter > k = Node k Nothing i m (delete s d)
delete s@(caracter:clave) (Node k v@(Just _) i m d) | caracter == k = cleanup (Node k v i (delete clave m) d)
                                                    | caracter < k = cleanup (Node k v (delete s i) m d)
                                                    | caracter > k = cleanup (Node k v i m (delete s d))

--keys :: devuelve una lista ordenada con las claves de un TTree
keys :: TTree k v -> [[k]]
keys E = []
keys (Leaf k v) = [[k]]
keys (Node k Nothing i m d) = keys i ++ map (k:) (keys m) ++ keys d
keys (Node k (Just _) i m d) = keys i ++ [[k]] ++ map (k:) (keys m) ++ keys d
