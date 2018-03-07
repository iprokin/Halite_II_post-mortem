module General where

nonIntRem :: RealFrac a => a -> a -> a
nonIntRem x y = x - (y * (fromIntegral $ truncate (x/y)))

symAngle :: (Floating a, Ord a) => a -> a
symAngle beta
  | pi < beta  = beta - 2*pi
  | -pi > beta = beta + 2*pi
  | otherwise  = beta

argmin :: Ord b => (a -> b) -> [a] -> a
argmin _ []  = error "Empty list!!!"
argmin _ [x] = x
argmin f (x:xs)
    | f x <= f argminrest = x
    | otherwise = argminrest
    where argminrest = argmin f xs

argmax :: Num b => Ord b => (a -> b) -> [a] -> a
argmax f xs = argmin (\x -> - f x) xs

argfirstmin :: Ord a => (b -> a) -> [b] -> b
argfirstmin _ [] = error "Empty list!!!"
argfirstmin _ [x0] = x0
argfirstmin f [x0,x1]
  | f x0 >= f x1 = x1
  | f x0 <  f x1 = x0
argfirstmin f (x0:x1:xs)
  | f x0 >= f x1 = argfirstmin f (x1:xs)
  | f x0 <  f x1 = x0

{- f determines weights -}
meanBy :: (Fractional a, Eq a) => (a -> a) -> [a] -> a
meanBy f xs
  | sumw == 0 = sum xs / (fromIntegral . length) xs
  | otherwise = sum (zipWith (*) xs ws) / sumw
    where ws = map f xs
          sumw = sum ws

-- | Removes small intervals [x-dx, x+dx] centered at points x in xs (sorted)
-- from wider interval [a, b]. Returns allowed intervals from [a, b] 
excludeIntervals :: (Num a, Ord a) => (a, a) -> [a] -> a -> [(a,a)]
excludeIntervals (a,b) xs dx = go $ (a-dx):xs ++ [(b+dx)]
    where go (x1:x2:xs)
            | a <= down && down < up && up <= b = (down, up) : go (x2:xs)
            | otherwise                         = go (x2:xs)
            where down = x1 + dx
                  up   = x2 - dx
          go [_]        = []
          go []         = []

argAndApp :: (a -> b) -> a -> (a, b)
argAndApp f a = (a, f a)

stateMapInc :: ([a] -> b -> (a, c)) -> [a] -> [b] -> [c]
stateMapInc _ _ [] = []
stateMapInc f states (x:xs) = res : stateMapInc f (snew:states) xs
    where (snew, res) = f states x

stateMap ::  (s -> b -> (s, c)) -> s -> [b] -> [c]
stateMap _ _ [] = []
stateMap f st (x:xs) = res : stateMap f newst xs
    where (newst, res) = f st x

scale01 :: Num a => a -> a -> a -> a
scale01 a b x = a + (b-a) * x

decayToFrom :: Floating a => a -> a -> a -> a
decayToFrom a b x = scale01 a b (exp (-x))

