module SymbolicVector where

import Graph




newtype SymbolicVector = SymbolicVector [Int]




symbolicVector :: [Graph] -> Graph -> SymbolicVector
symbolicVector keys g = SymbolicVector [ subgraphCount key g | key <- keys ]



