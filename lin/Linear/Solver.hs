{-# LANGUAGE ForeignFunctionInterface #-}

{- |

This, and glpk.c, is almost entirely based off of the hmatrix-glpk, licensed under GPL v3.0
The only change to the original is to ensure only integer solutions.
<https://github.com/haskell-numerics/hmatrix/blob/master/packages/glpk/src/Numeric/LinearProgramming.hs#L174

-}

module Linear.Solver(intopt) where

import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra.Devel hiding (Dense)
import Foreign(Ptr)
import System.IO.Unsafe(unsafePerformIO)
import Foreign.C.Types
import Data.List((\\),sortBy,nub)
import Data.Function(on)
import Numeric.LinearProgramming

glpFR, glpLO, glpUP, glpDB, glpFX :: Double
glpFR = 0
glpLO = 1
glpUP = 2
glpDB = 3
glpFX = 4

adapt :: Optimization -> (Int, Double, [Double])
adapt opt = case opt of
    Maximize x -> (sz x, 1 ,x)
    Minimize x -> (sz x, -1, (map negate x))
  where
    sz x | null x = error "simplex: objective function with zero variables"
         | otherwise = length x

extract :: Double -> Vector Double -> Solution
extract sg sol = r where
    z = sg * (sol!1)
    v = toList $ subVector 2 (size sol -2) sol
    r = case round (sol!0)::Int of
          1 -> Undefined
          2 -> Feasible (z,v)
          3 -> Infeasible (z,v)
          4 -> NoFeasible
          5 -> Optimal (z,v)
          6 -> Unbounded
          _ -> error "simplex: solution type unknown"

obj :: Bound t -> t
obj (x :<=: _)  = x
obj (x :>=: _)  = x
obj (x :&: _)  = x
obj (x :==: _) = x
obj (Free x)   = x

withObj :: Bound t -> t -> Bound t
withObj (_ :<=: b) x = (x :<=: b)
withObj (_ :>=: b) x = (x :>=: b)
withObj (_ :&: b) x = (x :&: b)
withObj (_ :==: b) x = (x :==: b)
withObj (Free _) x = Free x

tb :: Bound t -> Double
tb (_ :<=: _)  = glpUP
tb (_ :>=: _)  = glpLO
tb (_ :&: _)  = glpDB
tb (_ :==: _) = glpFX
tb (Free _)   = glpFR

lb :: Bound t -> Double
lb (_ :<=: _)     = 0
lb (_ :>=: a)     = a
lb (_ :&: (a,_)) = a
lb (_ :==: a)    = a
lb (Free _)      = 0

ub :: Bound t -> Double
ub (_ :<=: a)     = a
ub (_ :>=: _)     = 0
ub (_ :&: (_,a)) = a
ub (_ :==: a)    = a
ub (Free _)      = 0

mkBound1 :: Bound t -> [Double]
mkBound1 b = [tb b, lb b, ub b]

mkBound2 :: Bound t -> (t, [Double])
mkBound2 b = (obj b, mkBound1 b)

mkBounds :: Int -> [Bound [a]] -> [Bound Int] -> Matrix Double
mkBounds n b1 b2 = fromLists (cb++vb) where
    gv' = map obj b2
    gv | nub gv' == gv' = gv'
       | otherwise = error $ "simplex: duplicate bounds for vars " ++ show (gv'\\nub gv')
    rv | null gv || minimum gv >= 0 && maximum gv <= n = [1..n] \\ gv
       | otherwise = error $ "simplex: bounds: variables "++show gv++" not in 1.."++show n
    vb = map snd $ sortBy (compare `on` fst) $ map (mkBound2 . (:>=: 0)) rv ++ map mkBound2 b2
    cb = map mkBound1 b1

mkConstrD :: Int -> [Double] -> [Bound [Double]] -> Matrix Double
mkConstrD n f b1 | ok = fromLists (ob ++ co)
                 | otherwise = error $ "simplex: dense constraints require "++show n
                                     ++" variables, given " ++ show ls
    where
       cs = map obj b1
       ls = map length cs
       ok = all (==n) ls
       den = fromLists cs
       ob = map (([0,0]++).return) f
       co = [[fromIntegral i, fromIntegral j,den `atIndex` (i-1,j-1)]| i<-[1 ..rows den], j<-[1 .. cols den]]

mkConstrS :: Int -> [Double] -> [Bound [(Double, Int)]] -> Matrix Double
mkConstrS n objfun b1 = fromLists (ob ++ co) where
    ob = map (([0,0]++).return) objfun
    co = concat $ zipWith f [1::Int ..] cs
    cs = map obj b1
    f k = map (g k)
    g k (c,v) | v >=1 && v<= n = [fromIntegral k, fromIntegral v,c]
              | otherwise = error $ "simplex: sparse constraints: variable "++show v++" not in 1.."++show n

intopt :: Optimization -> Constraints -> Bounds -> Solution
intopt opt (Dense   []) bnds = intopt opt (Sparse []) bnds
intopt opt (Sparse  []) bnds = intopt opt (Sparse [Free [0#1]]) bnds
intopt opt (General []) bnds = intopt opt (Sparse [Free [0#1]]) bnds

intopt opt (Dense constr) bnds = extract sg sol where
    sol = intoptSparse m n (mkConstrD sz objfun constr) (mkBounds sz constr bnds)
    n = length objfun
    m = length constr
    (sz, sg, objfun) = adapt opt

intopt opt (Sparse constr) bnds = extract sg sol where
    sol = intoptSparse m n (mkConstrS sz objfun constr) (mkBounds sz constr bnds)
    n = length objfun
    m = length constr
    (sz, sg, objfun) = adapt opt

intopt opt constr@(General _) bnds = intopt opt (sparseOfGeneral constr) bnds

foreign import ccall unsafe "c_intopt_sparse" c_intopt_sparse
    :: CInt -> CInt                  -- rows and cols
    -> CInt -> CInt -> Ptr Double    -- coeffs
    -> CInt -> CInt -> Ptr Double    -- bounds
    -> CInt -> Ptr Double            -- result
    -> IO CInt                       -- exit code



intoptSparse :: Int -> Int -> Matrix Double -> Matrix Double -> Vector Double
intoptSparse m n c b = unsafePerformIO $ do
    s <- createVector (2+n)
    (cmat c `applyRaw` (cmat b `applyRaw` (s `applyRaw` id))) (c_intopt_sparse (fi m) (fi n)) #|"c_intopt_sparse"
    return s