{-# LANGUAGE UnicodeSyntax #-}

module Humbug.Utils.Map
( (∪)
) where

import Data.Map(Map, union, empty)

(∪) ∷ Ord k ⇒ Map k v → Map k v → Map k v
(∪) = union
