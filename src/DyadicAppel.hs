module DyadicAppel where
  import Data.List as List (take, drop, elemIndex)
  import System.Random as Random
  import Data.Vector as Vector hiding (fromList, fromLists, toList, toLists)
  import Data.Matrix as Matrix
  import MonadicAppel as Monadic

  {- TODO: this would be nice to use
  dyadicMatrixOperation :: (a -> a -> a) -> Matrix a -> Matrix a -> Maybe (Matrix a)
  dyadicMatrixOperation f matA matB =
    if (Monadic.shape matA) == (Monadic.shape matB)
    then Just (elementwiseUnsafe (f) matA matB)
    else Nothing
  -}

  add :: Matrix Double -> Matrix Double -> Maybe (Matrix Double)
  add matA matB = -- dyadicMatrixOperation (+)
    if (Monadic.shape matA) == (Monadic.shape matB)
    then Just (elementwise (+) matA matB)
    else Nothing

  subtract :: Matrix Double -> Matrix Double -> Maybe (Matrix Double)
  subtract matA matB = -- dyadicMatrixOperation (-)
    if (Monadic.shape matA) == (Monadic.shape matB)
    then Just (elementwise (-) matA matB)
    else Nothing

  multiply :: Matrix Double -> Matrix Double -> Maybe (Matrix Double)
  multiply matA matB = -- dyadicMatrixOperation (*)
    if (Monadic.shape matA) == (Monadic.shape matB)
    then Just (elementwise (*) matA matB)
    else Nothing

  divide :: Matrix Double -> Matrix Double -> Maybe (Matrix Double)
  divide matA matB =  -- dyadicMatrixOperation (/) -- TODO: how to catch divide by zero?
    if (Monadic.shape matA) == (Monadic.shape matB)
    then Just (elementwise (/) matA matB)
    else Nothing

  exponentiation = (**)

  circle :: Floating a => Integer -> Maybe (a -> a)
  circle 1 = Just (sin)
  circle 2 = Just (cos)
  circle 3 = Just (tan)
  circle 5 = Just (sinh) -- TODO: wikipedia skips 4; is that legit?
  circle 6 = Just (cosh)
  circle 7 = Just (tanh)
  circle (-1) = Just (asin)
  circle (-2) = Just (acos)
  circle (-3) = Just (atan)
  circle (-5) = Just (asinh)
  circle (-6) = Just (acosh)
  circle (-7) = Just (atanh)
  circle _ = Nothing

  deal :: Integer -> Integer -> IO [a]
  deal n max = undefined -- TODO

  epsilon :: Eq a => Matrix a -> Matrix a -> Matrix Int
  epsilon needles haystack =
    let (d1, d2) = Monadic.shape needles
        ndlList = toList needles
        hayList = toList haystack
        isPresent = \ ndl -> if ndl `Prelude.elem` hayList then 1 else 0
        binList = Prelude.map (isPresent) ndlList
    in fromList d1 d2 binList

  -- ceiling = max

  -- floor = min

  reshape :: Matrix a -> Matrix b -> Matrix b
  reshape shapeMat dataMat =
    let (d1, d2) = Monadic.shape shapeMat
        flatData = toList dataMat
    in fromList d1 d2 flatData -- XXX: maybe use setSize

  take :: Int -> Matrix a -> [a]
  take n mat =
    let flatMat = toList mat
    in List.take n flatMat

  drop :: Int -> Matrix a -> [a]
  drop n mat =
    let flatMat = toList mat
    in List.drop n flatMat

  decode = undefined -- TODO

  encode = undefined -- TODO

  residue = (flip (mod))

  catenation = (<|>)

  expansion = undefined -- TODO: maybe use extendTo?

  compression = undefined -- TODO

  iota :: Eq a => a -> Matrix a -> Maybe (Int, Int)
  iota needle haystack =
    let hayList = toList haystack
        loc =  needle `List.elemIndex` hayList
    in  case loc of
        Nothing -> Nothing
        -- XXX: test this!!!
        Just n  -> Just (n `mod` (nrows haystack), n `mod` (ncols haystack))

  matrixDivide :: Matrix a -> Matrix a -> Maybe (Vector a)
  matrixDivide matA matB = undefined

  rotation1 :: a -> b -> c
  rotation1 = undefined -- TODO

  rotation2 :: a -> b -> c
  rotation2 = undefined -- TODO

  -- logBase given by Prelude

  dyadicFormat :: Matrix Char -> Matrix a -> Matrix Char
  dyadicFormat = undefined -- TODO

  transpose :: Matrix a -> Matrix b -> Matrix b
  transpose = undefined -- TODO

  -- TODO: comparison and boolean ops
  -- make a special wrapper so they work with atoms, lists, vectors, and
  -- matrices, too?
