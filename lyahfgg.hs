(&&&) :: Bool -> Bool -> Bool
(&&&) True True = True
(&&&) _ _		= False

-- disjoint data type
data IntOrBool = BoolKind Bool | IntKind Integer deriving (Show)
getIntFromBoolOrBoolFromBool :: IntOrBool -> Integer
getIntFromBoolOrBoolFromBool (BoolKind a) = 1
getIntFromBoolOrBoolFromBool (IntKind a) = a


-- cart. pro. type
data Mercedes = C180 | C220 | C250 deriving (Show)
data CarColor = Red | Green | Yellow | Pink
data MercedesDescription = MercedesDesc Mercedes CarColor deriving (Show)

isMyMercedesCool :: MercedesDescription -> String
isMyMercedesCool (MercedesDesc _ Pink) = "Dude WTF? A pink Mercedes?" -- The uncool guys
isMyMercedesCool (MercedesDesc _ _) = "Wow, bro! Cool car." -- The cool guys

makeMyMercedesCool :: MercedesDescription -> MercedesDescription
makeMyMercedesCool (MercedesDesc a color)
					| color == Pink = MercedesDesc a Red
					| otherwise = MercedesDesc a color

-- cart. pro. type mixed with non cart. pro. type
data ShapeType = Rectangle Integer Integer | Circle Integer | Point
calculateArea :: ShapeType -> Integer
calculateArea (Rectangle a b) = a * b
calculateArea (Circle r) = r * 3
calculateArea (Point) = 0


-- Maybe / Either
getTranslationIfPossible :: Maybe a -> Maybe b -> Maybe (a,b)
getTranslationIfPossible a Nothing =  Nothing
getTranslationIfPossible Nothing b =  Nothing
getTranslationIfPossible (Just a) (Just b) = Just(a, b)

-- Tuples, pairs and stuff
data TupleType a b = TupleData a b deriving (Show)

firsty :: (a, b) -> a
firsty (a, b) = a

-- Vector examples
normOfVector :: (Double, Double, Double) -> Double
normOfVector (x1, x2, x3) = sqrt(x1^2 + x2^2 + x3^2)

negateVector :: (Double, Double, Double) -> (Double, Double, Double)
negateVector (x1, x2, x3) = (-x1, -x2, -x3)

addVector :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
addVector (xa1, xa2, xa3) (xb1, xb2, xb3) = (xa1 + xb1, xa2 + xb2, xa3 + xb3)

-- Higher order functions
twice :: (Integer -> Integer) -> Integer -> Integer
twice f x = 2 * f(x)


-- curry and uncurry
ccurry :: ((a, b) -> f) -> a -> b -> f
ccurry f a b = f(a, b)

unccurry :: (a -> b -> f) -> ((a, b) -> f)
unccurry f(a, b) = f a b


-- (.), even, odd, not, b2i
punkt :: (c -> d) -> (b -> c) -> b -> d
punkt f g x = f(g x)

isEven :: Int -> Bool
isEven a = a `mod` 2 == 0

isOdd :: Int -> Bool
isOdd a = not (isEven a)

makeNot :: Bool -> Bool
makeNot True = False
makeNot False = True

b2i :: Bool -> Int
b2i True = 1
b2i False = 0

doFlip :: (a -> b -> f) -> (b -> a -> f)
doFlip f a b = f b a

data Vector = Vec2 Int Int | Vec3 Int Int Int deriving (Show)

plusV :: Vector -> Vector -> Vector
plusV (Vec2 a b) (Vec2 x y) = Vec2 (a+x) (b+y)
plusV (Vec3 a b c) (Vec3 x y z) = Vec3 (a+x) (b+y) (c+z)

negV :: Vector -> Vector
negV (Vec3 a b c) = Vec3 (-a) (-b) (-c)
negV (Vec2 a b) = Vec2 a b

minusV :: Maybe Vector -> Maybe Vector -> Maybe Vector
minusV (Just(Vec2 a b)) (Just(Vec2 x y)) = Just(Vec2 (a-x) (b-y))
minusV (Just(Vec3 a b c)) (Just(Vec3 x y z)) = Just(Vec3 (a-x) (b-y) (c-z))
minusV Nothing _ = Nothing
minusV _ Nothing = Nothing

-- where
increment :: Int -> Int
increment a = aIncrement
	where
		aIncrement = a + 1
-- let
solve01 :: Double -> Double -> (Double, Double)
solve01 p q =
 let pOver2 = p / 2
     root = sqrt (pOver2^2 - q)
 in (-pOver2 + root, -pOver2 - root)

-- let
solve :: Double -> Double -> (Double, Double)
solve p q =
 let {
 	pOver2 = p / 2;
 	root = sqrt (pOver2^2 - q);
 } in (-pOver2 + root, -pOver2 - root)

solve03 :: Double -> Double -> Maybe (Double, Double)
solve03 p q
	| ((p / 2)^2 - q > 0) = Just (solve p q)
	| otherwise = Nothing

areaOfTwoCricles :: Double -> Double -> Double
areaOfTwoCricles a b =
	let areaA = area a
	    areaB = area b
	    area r = pi * r^2
	in areaA + areaB

areaOfTwoCricles2 :: Double -> Double -> Double
areaOfTwoCricles2 a b =
	let { (a1, a2, area) =
		(area a, area b, \r -> pi * r^2); }
	in a1 + a2

-- What is the type of x?
-- Zero :: ZeroOneTwo
-- One :: a -> ZeroOneTwo a
-- Two :: a -> a -> ZeroOneTwo a
data ZeroOneTwo a = Zero | One a | Two a a deriving(Show)

-- that gives Zero, or One solution, or Two solutions
-- to the quadratic equation, depending on the actual situation.
solve04 :: Double -> Double -> ZeroOneTwo Double
solve04 p q
	| ((p / 2)^2 - q > 0) = Two (-pOver2 + root) (-pOver2 - root)
	| ((p / 2)^2 - q == 0) = One (-(p / 2))
	| otherwise = Zero
		where {
			pOver2 = p / 2;
    		root = sqrt (pOver2^2 - q);
	}

divForSure :: Maybe Int -> Int
divForSure (Just x)
       | even x 	= div x 2
       | otherwise 	= div (x-1) 2
divForSure Nothing = 0

-- bubble sort
bsort :: Ord a => [a] -> [a]
bsort s = case _bsort s of
               t | t == s    -> t
                 | otherwise -> bsort t
  where _bsort (x:x2:xs) | x > x2    = x2:(_bsort (x:xs))
                         | otherwise = x:(_bsort (x2:xs))
        _bsort s = s

sqrtF :: Double -> Maybe Double
sqrtF x
  | x < 0     = Nothing
  | otherwise = Just (sqrt x)

recipF :: Double -> Maybe Double
recipF x
  | x == 0    = Nothing
  | otherwise = Just (recip x)

-- kleisli01, kleisli02 :: Double -> Maybe Double
-- kleisli01 = sqrtF >=> recipF
-- kleisli02 = recipF >=> sqrtF
-- 		• 	kleisli01 x first applies sqrtF to x.
-- 		• 	If the result is Nothing, the result of kleisli01 x is Nothing.
-- 		• 	If the result is Just a value, recipF is applied to this value,
--			and the result is the result of kleisli01 x.
(>=>) g f x
	| ((f(x) /= Nothing) == True) = g((\(Just x) -> x) (f x))
	| otherwise = Nothing

-- .. or by using "case"; same as (>=>)
(>==>) g f x =
	case g x of
			Just y -> f y
			Nothing	-> Nothing

-- equals to fst from prelude
foapFirstOfAPair :: (a, b) -> a
foapFirstOfAPair (x, _) = x

-- type constraint
-- only works on stuff that is actually sortable
-- (for all possible types a that are instances of class Ord)
sortG :: Ord a => (a, a) -> (a, a)
sortG (x, y)
  | x <= y    = (x, y)
  | otherwise = (y, x)

getValueIfEquals :: Eq a => a -> a -> Maybe a
getValueIfEquals x y
	| x == y = Just x
	| True = Nothing

ccompare :: (Ord a) => a -> a -> Ordering
ccompare x y
      | x == y    = EQ
      | x <= y    = LT
      | otherwise = GT

instance Eq CarColor where
	-- equality
	Red == Red = True
	Yellow == Yellow = True
	Green == Green = True
	Pink == Pink = True
	_ == _ = False
	-- not equal
	x /= y = not (x == y)

instance Show CarColor where
	show  Red = " is Red"
	show  Yellow = " is Yellow"
	show  Green = " is Green"
	show  Pink = " is Pink"

sortPairPredict :: (Ord a) => (a, a) -> (a, a)
sortPairPredict (x, y)
  | x <= y    = (x, y)
  | otherwise = (y, x)

notEqual :: (Eq a) => a -> a -> Bool
notEqual a b
	| a == b = False
	| otherwise = True

greaterEqual :: (Ord a) => a -> a -> Bool
greaterEqual a b
	| (compare a b == GT) = True
	| otherwise = False

data Mod3 = Zero3 | One3 | Two3 deriving (Show, Eq)
data Mod12 = M Int

-- Throws an exception if I try
-- to use a not implemented method
instance Num Mod3 where
	-- (+) :: Mod3 -> Mod3 -> Mod3
	Zero3 + x = x
	x + Zero3 = x
	One3 + One3 = Two3
	One3 + Two3 = Zero3
	Two3 + One3 = Zero3
	Two3 + Two3 = One3
	-- (*) :: Mod3 -> Mod3 -> Mod3
	Zero3 * x = Zero3
	x * Zero3 = Zero3
	One3 * x = x
	x * One3 = x
	Two3 * Two3 = One3
	-- negate x = 0 - x
	negate Zero3 = Zero3
	negate One3 = Two3
	negate Two3 = One3
	fromInteger x =
		case x `mod` 3 of
			0 -> Zero3
			1 -> One3
			2 -> Two3
	abs x = x
	signum x = x

instance Num Mod12 where
	(M x) + (M y) = M((x + y) `mod` 12)
	(M x) * (M y) = M((x * y) `mod` 12)
	(M x) - (M y) = M((x - y) `mod` 12)
	fromInteger x = M(fromInteger x `mod` 12)
	abs x = x
	signum x = x

fib :: (Num a, Eq a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fac :: Int -> Int
fac 0 = 1
fac n
  | n <= 0 = 0
  | otherwise = n * fac (n - 1)

data L1 = Nil1
data L2 a = Cons2 a L1 						-- Cons2 :: a -> L1 -> L2 a
data L3 a = Cons3 a (L2 a)					-- Cons3 :: a -> L2 a -> L3 a
data L4 a = Cons4 a (L3 a)					-- Cons4 :: a -> L3 a -> L4 a
l11 = Nil1									-- l11 :: L1
l12 = Cons2 25 Nil1							-- l12 :: L2 Integer
l13 = Cons3 26 (Cons2 25 Nil1)				-- l13 :: L3 Integer
l14 = Cons4 27 (Cons3 26 (Cons2 25 Nil1))	-- l14 :: L4 Integer

-- my own uberlist
data List a = Nil | Cons a (List a) deriving(Show)
-- Types:
-- 		Nil :: List a
--  	Cons :: a -> List a -> List a
-- 		Cons (5 :: Int) :: List Int -> List Int
-- 		Cons 5 :: Num a => List a -> List a

-- Cons 5 (Cons 3 (Cons 6 Nil))
--                        |---| List a
-- 				        ||      Int
--                |-----------| List Int
--              ||              Int
-- 	       |------------------| List Int
-- 		 ||				        Int
-- |--------------------------| List Int
--

-- Prelude <list>
----------------------------
-- Prelude> :t []
--	[] :: [a]
--
-- Prelude> :t [1]
--	[1] :: Num t => [t]
--0
-- Prelude> :t [(1 :: Int)]
--	[(1 :: Int)] :: [Int]

mmax :: Ord a => List a -> a
mmax (Cons x Nil) = x
mmax (Cons x (Cons a b))
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = mmax (Cons a b)

hhead :: List a -> a
hhead (Cons a _) = a

sumList :: List Int -> Int
sumList Nil = 0
sumList (Cons x xs) = x + sumList xs
--            |                 		Int
--              ||              		List Int
--       |-------|              		List Int
--
--                  = x + sumList xs
--                    |         		Int
--								  ||    List Int
--                        |--------|    Int
--                    |------------|    Int
--
--------------------------------------------------
--
--      |########| << Pattern // Not an expression
--

repConst :: Int -> a -> List a
repConst 0 _ = Nil
repConst n c = Cons c(repConst (n-1) c)

-- Composition: (f°g)x = f(g(x))
funnyMult x y = sumList(repConst x y)
--              |-------------------|	(sumList ° repConst x) y
funnyMultWithComposition x = sumList.repConst x

prepend :: List a -> List a -> List a
prepend Nil Nil = Nil
prepend (Cons x xs) ys = Cons x(prepend xs ys)
prepend Nil ys = ys

reverseList :: List a -> List a
reverseList Nil = Nil
reverseList (Cons x xs) = prepend (reverseList xs)(Cons x Nil)

reverseListTailRecursion :: List a -> List a
reverseListTailRecursion xs = h xs Nil
	where {
	h(Cons x xs) accu = h xs (Cons x accu);
	h Nil accu = accu
}

sumLList :: [Int] -> Int
sumLList [] = 0
sumLList (x:xs) = x + sumLList xs

-- we can also let the compiler do it
data Position = TTop | BBottom | LLeft | RRight deriving (Show, Eq)

myMax :: Ord x => [x] -> x
myMax [] = error "Oppsie!"
myMax (x : []) = x
myMax (x : xs)
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = myMax xs

myMin :: Ord x => [x] -> x
myMin [] = error "Empty List"
myMin (x : []) = x
myMin (x : xs)
	| x < smallestX = x
	| otherwise = smallestX
	where smallestX = myMin xs

myHead :: [x] -> x
myHead [] = error "Empty List"
myHead (x : []) = x
myHead (x : xs) = x

myTail :: [x] -> [x]
myTail [] = error "Empty List"
myTail (x : []) = [x]
myTail (x : xs) = xs

myLast :: [x] -> x
myLast [] = error "Empty List"
myLast (x : []) = x
myLast (x : xs) = myLast xs

myInit :: [x] -> [x]
myInit [] = error "Empty List"
myInit (x : []) = []
myInit (x : xs) = [x] ++ myInit xs

myNull :: [x] -> Bool
myNull [] = True
myNull _ = False

myLength :: [x] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

-- See http://hackage.haskell.org/package/
-- base-4.6.0.1/docs/Prelude.html#v:map
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = error "Ooopsie. List is empty :("
myMap f (x : []) = [f x]
myMap f (x : xs) = (f x) : (myMap f xs) -- [f x] ++ (myMap f xs)

myFilter :: (x -> Bool) -> [x] -> [x]
myFilter _ [] = []
myFilter predi (x : [])
	| predi x = [x]
	| otherwise = []
myFilter predi (x : xs)
	| predi x = [x] ++ myFilter predi xs
	| otherwise = myFilter predi xs

myConcat :: [x] -> [x] -> [x]
myConcat [] [] = []
myConcat [] xs = xs
myConcat xs [] = xs
myConcat (xs) (y : ys) = ys

myIndex :: [x] -> Int -> x
myIndex (x : _) 0 = x
myIndex (x : xs) i = myIndex xs (i-1)

myTake :: Int -> [x] -> [x]
myTake 0 _ = []
myTake 1 (x : _) = [x]
myTake n (x : xs) = [x] ++ (myTake (n-1) xs)

myDrop :: Int -> [x] -> [x]
myDrop 0 (xs) = xs
myDrop 1 (x : xs) = xs
myDrop n (x : xs) = myDrop (n-1) xs

myZip :: [x] -> [y] -> [(x,y)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x,y) : (myZip xs ys)

-- Would result in [2,3,4,5]
mapped = map (+1) [1, 2, 3, 4]

-- Would result in [2,4,6,8]
filtered = filter (\x -> (mod) x 2 == 0)  [1, 2, 3, 4, 5, 6, 7, 8]

sumTupple :: [(Int, Char)] -> Int
sumTupple ((n, c) : []) = n
sumTupple (x : xs) = fst x + sumTupple xs

data Unit = Me | Kg | S deriving (Eq, Show)
type Value = Double
type Exponent = Int
type UnitExp = (Unit, Exponent)

data Quantity = Quant Value [UnitExp] deriving (Show)

norm :: [UnitExp] -> [UnitExp]
norm unitExps = f3 Me unitExps ++ f3 Kg unitExps ++ f3 S unitExps

f1 :: Unit -> [UnitExp] -> [UnitExp]
f1 unit = filter (\(u, _) -> u == unit)

f2 :: Unit -> [UnitExp] -> [Exponent]
f2 unit unitExps = map snd (f1 unit unitExps)

f3 :: Unit -> [UnitExp] -> [UnitExp]
f3 unit unitExps
	| exp == 0 = []
	| otherwise = [(unit, exp)]
	where exp = sum(f2 unit unitExps)

instance Eq Quantity where
	(Quant v1 ues1) == (Quant v2 ues2)
		| norm ues1 == norm ues2 = v1 == v2

instance Num Quantity where
	Quant v1 ues1 + Quant v2 ues2
		| norm ues1 == norm ues2 = Quant(v1 + v2)(norm ues1)
	Quant v1 ues1 * Quant v2 ues2 = Quant(v1 + v2)(norm(ues1 ++ ues2))
	negate (Quant v ues) = Quant (-v) (norm ues)
	abs (Quant v ues)
		| v > 0 = Quant v (norm ues)
		| otherwise = Quant (-v) (norm ues)
	signum (Quant v ues)
		| v == 0 = Quant 0 []
		| v > 0 = Quant 1 []
		| otherwise = Quant (-1) []
	fromInteger i = Quant (fromInteger i) []

-- removeEvenFromList
-- eg. "abcde" -> "bde"
removeEvenFromList [] = []
removeEvenFromList (x : []) = [x]
removeEvenFromList (x : xs)
	| (mod (length xs) 2) == 0 = removeEvenFromList xs
	| otherwise = x : (removeEvenFromList xs)

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f v [] = v
myFoldr f v (x : xs) = x `f` myFoldr f v xs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f v [] = v
myFoldl f v (x : xs) = myFoldl f (v `f` x) xs

-- Types: 
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- todo reverse, ++ and && implement with foldr and foldl implement.
and_foldr :: [Bool] -> Bool
and_foldr x = foldr (==) True x

and_foldl :: [Bool] -> Bool
and_foldl x = foldr (==) True x

reverse_foldl :: [a] -> [a]
reverse_foldl xs = foldl (\xs x -> x : xs) [] xs

reverse_foldr :: [a] -> [a]
reverse_foldr xs = foldr (\x xs -> xs ++ [x]) [] xs

reverse_flip :: [a] -> [a]
reverse_flip xs = foldl (flip (:)) [] xs

-- flip :: (a -> b -> c) -> b -> a -> c
flipV1 f = g
	where g x y = f y x
flipV2 f = \x y -> f y x
flipV3 f x y = f y x

concate_foldr :: [a] -> [a] -> [a]
concate_foldr xs ys = foldr (\x xs -> x : xs) xs ys

-- concate_foldl :: [a] -> [a] -> [a]

-- reverseAppend [3, 2, 1] [4, 5, 6] -> [1, 2, 3, 4, 5, 6]
reverseAppend :: [a] -> [a] -> [a]
reverseAppend xs ys = foldl (flip (:)) [] xs ++ ys

reverseAppendImproved :: [a] -> [a] -> [a]
reverseAppendImproved = flip(foldl(flip(:))) -- ???


main = do
	let myMercedes = MercedesDesc C180 Pink
	print (myMercedes)
	print (isMyMercedesCool myMercedes)
	let myNowCoolMercedes = (makeMyMercedesCool myMercedes)
	print (isMyMercedesCool myNowCoolMercedes)

	let me2 = ("Livio Bieri", 007)
	print (firsty me2)

	let me = TupleData "Livio Bieri" 17108623
	print(me)

	print(normOfVector (1,2,3))
	print(negateVector (1,2,3))
	print(addVector (1,2,3) (1,2,3))

	print(punkt id id 2)
	print((id.id) 2)

	print(isEven 1)
	print(isEven 2)

	print(isOdd 1)
	print(isOdd 2)

	let u = Vec3 1 2 3
	let v = Vec3 1 2 3
	print(plusV u v)
	print(negV u)
	print(minusV (Just u) (Just v))

	print(solve03 2 3)
	print(solve03 3 1)

	let maybeSomthingCool = (sqrtF >=> recipF) (16)
	print(maybeSomthingCool)

	let toBeSorted = (5, 2)
	print(sortG toBeSorted)

	print(fib 10)

	let myList = Cons 2(Cons 3(Cons 4 (Cons 0 Nil)))
	print(sumList myList)
	print(reverseList myList)
	print(mmax myList)

	let listA = 5 : 1 : 2 : [];
	print(listA)

	let listB = [5, 1, 2, 10, 230, 2220, 9999, -999, 2, 3, 56, 43, 0];
	print(listB)

	let sumLListA = sumLList(listA);
	print(sumLListA)

	print(listA ++ listB)

	print(myMax listB)
	print(myMin listB)

	print(myHead listB)
	print(myTail listB)

	print(myLast listB)
	print(myInit listB)

	print(myNull listB)
	print(myNull [])

	print(myLength listB)

	print(myMap (+3) listB)
	print(myFilter (odd) [5,6,7])

	print(myIndex listB 0)

	print(myTake 4 listB)
	print(myDrop 4 listB)

	print(myZip [1,2,3,4] [2,4,3,4,5,6])

	print(mapped)
	print(filtered)

	print(sumTupple [(1, 'c'), (2, 'f'), (333, 'f')])
	print(increment 1)

	print(areaOfTwoCricles 1 2)
	print(removeEvenFromList "abcde")

	print(foldr (+) 0 [1,2,3]) -- 6
	print(foldl (+) 0 [1,2,3]) -- 6

	print(foldl (&&) True [(\x -> x == 1)(1), True, True, odd(3), even(4)])

	print(and_foldl [True, True, True, False, True]) -- > False
	print(and_foldl [True, True, True, True, True]) -- > True

	print(and_foldr [True, True, True, False, True]) -- > False
	print(and_foldr [True, True, True, True, True]) -- > True

	print(reverse_foldl [1, 2, 3, 4, 5])
	print(reverse_foldr [1, 2, 3, 4, 5])
	print(reverse_flip [1, 2, 3, 4, 5])

	print(concate_foldr [1, 2, 3, 4, 5] [1, 2, 3, 4, 5])
	
	print(reverseAppend [3, 2, 1] [4, 5, 6])
	print(reverseAppendImproved [3, 2, 1] [4, 5, 6])