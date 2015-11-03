{-#LANGUAGE BangPatterns#-}

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric.LinearAlgebra (loadMatrix,toRows,toList)


main :: IO ()
main = do rawPoints <- loadMatrix "swissroll.dat"
          let listPoints = map (U.fromList . toList) . toRows $ rawPoints
              vectorPoints = V.map (U.fromList . toList)
                             . V.fromList . toRows $ rawPoints
          print $ seq (simpleBuildNew listPoints) "Done."
          print $ seq (simpleBuildOld vectorPoints) "Done."

simpleBuildNew :: [U.Vector Double] -> [Double]
simpleBuildNew = L.foldl' foldFunc [] . prep
  where
   prep             = init . init . L.tails
   distpart y       = tail . map (distFunc $ head y) $ y
   foldFunc !acc !x = (L.++) acc (distpart x)

simpleBuildOld :: V.Vector (U.Vector Double) -> V.Vector (U.Vector Double)
simpleBuildOld = V.unfoldr unfoldFunc
  where
    unfoldFunc x   = if V.null . V.tail $ x
                      then Nothing
                      else Just $ distVecPair  x
    distVecPair xs = (,) <$> uncurry dists <*> snd $ splitVec xs
    splitVec vec   = (V.head vec,V.tail vec)


distFunc ::  U.Vector Double -> U.Vector Double -> Double
distFunc xs ys = U.sum . U.zipWith diffsq xs $ ys
  where
    diffsq x y = (x-y) * (x-y)

dists
 :: U.Vector Double
 -> V.Vector (U.Vector Double)
 -> U.Vector Double
dists pt = U.fromList . V.toList . V.map (distFunc pt)
