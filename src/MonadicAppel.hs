module MonadicAppel where
  import System.Random as Random
  import Data.Matrix as Matrix

  roll :: Int -> IO Int
  roll max = Random.randomRIO (1, max)

  -- ceiling given by Prelude
  -- floor given by Prelude

  shape :: Matrix a -> (Int, Int)
  shape matrix = (nrows matrix, ncols matrix)

  -- (not) given by Prelude
  -- abs given by Prelude

  iota :: Int -> [Int]
  iota i = [1..i]

  -- exp given by Prelude
  -- negate given by Prelude
  -- id given by Prelude
  -- signum given by Prelude

  reciprocal :: Double -> Maybe Double
  reciprocal 0 = Nothing
  reciprocal n = Just (1 / n)

  reshape = (toList)

  inverse :: Num a => Matrix a -> Matrix a
  inverse mat = undefined -- TODO

  piTimes = (* pi)

  -- log given by Prelude
  
  reversal :: Matrix a -> Matrix a
  reversal mat = undefined -- TODO

  gradeBy :: (a -> a -> Ordering) -> Matrix a -> Matrix a
  gradeBy f mat = undefined -- TODO

  gradeUp :: Matrix a -> Matrix a
  gradeUp mat = undefined -- TODO

  gradeDown :: Matrix a -> Matrix a
  gradeDown mat = undefined -- TODO

  execute = undefined -- TODO

  format :: Matrix a -> String
  format mat = undefined -- TODO

  transpose :: Matrix a -> Matrix a
  transpose = Matrix.transpose

  factorial :: Integer -> Integer
  factorial n = foldr (*) 1 [1..n]
