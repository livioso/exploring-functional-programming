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
makeMyMercedesCool (MercedesDesc a Pink) = MercedesDesc a Red
makeMyMercedesCool a					 = a


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


solve01 :: Double -> Double -> (Double, Double)
solve01 p q =
 let pOver2 = p / 2
     root = sqrt (pOver2^2 - q)
 in (-pOver2 + root, -pOver2 - root)

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

data ZeroOneTwo a 
	= Zero
	| One a
	| Two a a
	deriving(Show)

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
	Red == Red = True
	Yellow == Yellow = True
	Green == Green = True
	Pink == Pink = True
	_ == _ = False

instance Show CarColor where
	show  Red = " is Red"
	show  Yellow = " is Yellow"
	show  Green = " is Green"
	show  Pink = " is Pink"

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

instance Num Mod12 where
	(M x) + (M y) = M((x + y) `mod` 12)
	(M x) * (M y) = M((x * y) `mod` 12)
	(M x) - (M y) = M((x - y) `mod` 12)
	fromInteger x = M(fromInteger x `mod` 12)

fib :: (Num a, Eq a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fac 0 = 1
fac n
  | n <= 0 = 0
  | otherwise = n * fac (n - 1)

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

mmax :: Ord a => List a -> a
mmax (Cons a Nil) = a

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

	let myList = Cons 2(Cons 3(Cons 4 Nil))
	print(sumList myList)
	print(reverseList myList)

	let listA = 5 : 1 : 2 : []; 
	print(listA)

	let listB = [5, 1, 2, 10, 230, 2220, 9999, -999, 2, 3, 56, 43, 0];
	print(listB)

	let sumLListA = sumLList(listA);
	print(sumLListA)

	print(myMax listB)
	print(myMin listB)

	print(listA ++ listB)