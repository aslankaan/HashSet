--Aslan Kaan YILMAZ
--040090592


module HashSetProject where


import Prelude
import Data.Char (ord)

data Set a = HashSet [(Int,[a])]


instance Show a => Show (Set a) where
  show (HashSet []) = "{}"
  show (HashSet  xs) =
      "{" ++ buckets2string xs ++ "}" where
      		buckets2string [] = ""
      		buckets2string ((_,x'):[]) = foldr1 (\x y -> x ++ "," ++ y) (map show x')
      		buckets2string ((_,x'):xs') =  foldr1 (\x y -> x ++ "," ++ y) (map show x') ++ "," ++ buckets2string xs'

hash :: Show a => a -> Int
hash x = calculate (show x) where
	calculate::String -> Int
	calculate [] = 0;
	calculate (c:cs) = mod (ord c + calculate cs) 10



empty :: Set a
empty = HashSet []

add2bucket [] x' = [x']
add2bucket b@(x:xs) x'
	| x == x'   = b
	| otherwise = x : add2bucket xs x'

add :: (Show a, Eq a) => Set a -> a -> Set a
add (HashSet h) x = HashSet (add' h x)

add' [] x 					= (hash x, x:[]) : []
add' (h@((k,elems)) : hs) x
	| k == (hash x) = (k , (add2bucket elems x)) : hs
	| otherwise     = h : (add' hs x)

makeSet :: (Eq a, Show a) => [a] -> Set a
makeSet = foldl add empty


contains :: (Eq a, Show a) => Set a -> a -> Bool
contains (HashSet h) x = contains' h x


contains' [] _ = False 
contains' (h@((k,elems)) : hs) x
	| k == (hash x) = searchBucket elems x
	| otherwise		= contains' hs x
		where
			searchBucket [] x' = False
			searchBucket (e:es) x'
				| e == x' = True
				| otherwise = searchBucket es x'


union :: (Eq a,Show a) => Set a -> Set a -> Set a
union (HashSet h1) (HashSet h2) = HashSet (union' h1 h2)

union' xs [] = xs
union' [] ys = ys
union' xs ((k,es) : hs) = union' (addAll xs es) hs
	where
		addAll xs' [] 		= xs'
		addAll xs' (e':es') = addAll (add' xs' e') es'


card :: Set a -> Int
card (HashSet xs) = card' xs

card' [] = 0
card' ((k,elems) : hs) = (length elems) + (card' hs)





















