
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
  
class Grid g where
  type Dimension g :: *
  data CAState g :: *
  type GridState g :: *
       
  lookupCell :: g ->  Dimension g -> GridState g -> CAState g       
  gridNeighbours :: g -> (Dimension g) -> [Dimension g]  
  transition :: g -> CAState g -> [CAState g] -> CAState g
  gridCells0 :: g -> GridState g 

  cellState :: (Ord (Dimension g), Show (Dimension g), Num t, Ord t, Show t, Eq t) =>  
               g -> t -> Dimension g -> CAState g
  cellState g 0 d = lookupCell g d $ gridCells0 g
  cellState g t d 
      | (t < 0) = error $ "cellState t must not be < 0, was: " ++ show t
      | otherwise = let t' = t - 1
                        s' = cellState g t' d
                        ns = gridNeighbours g d
                        nss' = map (cellState g t') ns
                    in transition g s' nss'

data Rule30 = Rule30 [CAState Rule30]

instance Grid Rule30 where
  type Dimension Rule30 = Int
  data CAState Rule30 = Rule30Cell Bool
                      deriving (Show)
  type GridState Rule30 = [CAState Rule30]
  lookupCell (Rule30 css) i cs
    | (i < 0) = Rule30Cell False
    | (i >= (length css)) = Rule30Cell False
    | otherwise = cs !! i  
  gridNeighbours _ d = [d-1, d+1]
  transition (Rule30 l) cs nss = t' (nss !! 0) cs (nss !! 1)
      where t' (Rule30Cell True) (Rule30Cell True) (Rule30Cell True) = Rule30Cell False
            t' (Rule30Cell True) (Rule30Cell True) (Rule30Cell False) = Rule30Cell False
            t' (Rule30Cell True) (Rule30Cell False) (Rule30Cell True) = Rule30Cell False
            t' (Rule30Cell True) (Rule30Cell False) (Rule30Cell False) = Rule30Cell True
            t' (Rule30Cell False) (Rule30Cell True) (Rule30Cell True) = Rule30Cell True
            t' (Rule30Cell False) (Rule30Cell True) (Rule30Cell False) = Rule30Cell True
            t' (Rule30Cell False) (Rule30Cell False) (Rule30Cell True) = Rule30Cell True
            t' (Rule30Cell False) (Rule30Cell False) (Rule30Cell False) = Rule30Cell False

              
  gridCells0 (Rule30 css) = css


css = map Rule30Cell [True, False, False, False, True, True, False, True]
tg = Rule30 css

a = [ cellState tg 12 i | i <- [0 .. length css]] 

main = print a

 
