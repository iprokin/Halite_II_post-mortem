module ListFinite
( mkListFinite
, enlistFinite
, ListFinite
, headLF
, (.!)
, getList
) where

data ListFinite l i = ListFinite i [l] i

instance (Show l, Show i) => Show (ListFinite l i) where
    show (ListFinite n xs _) = "ListFinite "++show n++" "++show xs

mkListFinite :: Int -> [a] -> ListFinite a Int
mkListFinite maxLen xs =
    ListFinite maxLen ys (fromIntegral $ length ys)
        where ys | length xs <= maxLen = xs
                 | otherwise           = take maxLen xs

enlistFinite :: l -> ListFinite l Int -> ListFinite l Int
enlistFinite x (ListFinite maxLen xs i)
  | (i+1) <= maxLen = ListFinite maxLen (x:xs) (i+1)
  | otherwise       = ListFinite maxLen (x: (take (maxLen-1) xs)) maxLen

headLF :: ListFinite l i -> l
headLF (ListFinite _ (x:_) _) = x

(.!) :: ListFinite a b -> Int -> a
(.!) (ListFinite _ xs _) n = xs !! n

getList :: ListFinite a b -> [a]
getList (ListFinite _ xs _) = xs
