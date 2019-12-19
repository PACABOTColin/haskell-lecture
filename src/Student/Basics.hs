module Student.Basics where


-- | Write a functions that adds two Double.
add :: Double -> Double -> Double
add a b = a + b

-- | Write a functions that divides two Double.
divide :: Double -> Double -> Double
divide a b = a / b

-- This is a sum type. A value of type "DoubleResult" can either be
-- a DoubleSuccess containing a Double, or a DoubleFailure.
--
-- This will be used to identify operations that fail (for example, because
-- of a divide by 0 operation).
data DoubleResult
    = DoubleSuccess Double
    | DoubleFailure
    deriving (Show, Eq)

-- | Rewrite a function that divides two Doubles, but that explicitely
-- fails when it divides by 0, or explicitely succeeds.
divide' :: Double -> Double -> DoubleResult
divide' a b = case b of
  0 -> DoubleFailure
  _ -> DoubleSuccess (a / b)

-- | This is a generic (parametric) type. It represents something that can
-- either fail, or succeed with a value of type "a".
data Result a
    = Success a
    | Failure
    deriving (Show, Eq)

-- | Write this function again, but this time using the generic type.
divide'' :: Double -> Double -> Result Double
divide'' a b = case b of
  0 -> Failure
  _ -> Success (a / b)

-- | Represent the kind of expressions one can compute on a pocket
-- calculator.
data Operation
    = Add Operation Operation
    | Sub Operation Operation
    | Mul Operation Operation
    | Div Operation Operation
    | Value Double
    deriving (Show, Eq)

-- | This is (5 * 3.4) + 8
example1 :: Double
example1 = eval (Add (Mul (Value 5) (Value 3.4)) (Value 8))

-- | What is this ?
example2 :: Double
example2 = eval (Div (Div (Value 5) (Value 3.4)) (Value 0))

-- | From an operation, compute its value.
eval :: Operation -> Double
eval ope = case ope of
  Add a b -> eval a + eval b
  Sub a b -> eval a - eval b
  Mul a b -> eval a * eval b
  Div a b -> eval a / eval b
  Value a -> a

-- | Try it in the REPL with example2!
-- Now write the correct function:
eval' :: Operation -> Result Double
eval' ope = case ope of
  Add a b -> case (eval' a, eval' b) of
    (Failure, _) -> Failure
    (_, Failure) -> Failure
    (Success c, Success d) -> Success (c + d)
  Sub a b -> case (eval' a, eval' b) of
    (Failure, _) -> Failure
    (_, Failure) -> Failure
    (Success c, Success d) -> Success (c - d)
  Mul a b -> case (eval' a, eval' b) of
    (Failure, _) -> Failure
    (_, Failure) -> Failure
    (Success c, Success d) -> Success (c * d)
  Div a b -> case (eval' a, eval' b) of
    (_, Success 0) -> Failure
    (Failure, _) -> Failure
    (_, Failure) -> Failure
    (Success c, Success d) -> Success (c / d)
  Value a -> Success a

example1' :: Result Double
example1' = eval' (Add (Mul (Value 5) (Value 3.4)) (Value 8))

example2' :: Result Double
example2' = eval' (Add (Div (Value 5) (Value 3.4)) (Value 0))

-- | A (linked) list is either empty, or contains an element (head) and a list (tail).
data List a
    = Empty
    | Cons a (List a)
    deriving Show

-- | Returns the first element of a list.
listHead :: List a -> Result a
listHead list = case list of
  Empty -> Failure
  Cons value next -> Success value
-- listHead :: List a -> Result a
-- listHead list = case list of
--   Empty -> Failure
--   Cons item b-> case b of
--     Empty -> Success item
--     Cons value next -> listHead(next)


head1 :: Result Int
head1 = listHead Empty -- Failure
head2 :: Result Int
head2 = listHead (Cons 5 (Cons 4 Empty)) -- success 5

-- | Returns the tail of a list.
listTail :: List a -> Result (List a)
listTail list = case list of
  Empty -> Failure
  Cons value next -> Success next

tail1 :: Result (List Int)
tail1 = listTail Empty -- Failure
tail2 :: Result (List Int)
tail2 = listTail (Cons 5 (Cons 4 Empty)) -- success (Cons 4 Empty)

-- | Sum of all integers in a list.
listSum :: List Int -> Int
listSum list = case list of
  Empty -> 0
  Cons value next -> value + listSum(next)

-- | Compare two lists for equality.
listEq :: Eq a => List a -> List a -> Bool
listEq listA listB = case (listA, listB) of
  (Empty, Empty) -> True
  (Empty, _) -> False
  (_, Empty) -> False
  (Cons valueA nextA, Cons valueB nextB) -> valueA == valueB && listEq nextA nextB

-- | Converts our list type into Haskell's built-in list type.
toList :: List a -> [a]
toList list = case list of
  Empty -> []
  Cons value next -> value : (toList(next))

-- Given a function, converts all elements of a list.
lmap :: (a -> b) -> List a -> List b
lmap funct list = case list of
  Empty -> Empty
  Cons value next -> Cons (funct value) (lmap funct next)

-- | Black magic!
-- Uncomment the relevant test if you wrote it!

  -- fmap :: (a -> b) -> f a -> f b
  -- pure :: a -> f a
  -- (<*>) :: f (a -> b) -> f a -> f b

-- ltraverse :: Applicative f => (a -> f b) -> List a -> f (List b)
-- ltraverse funct list = 


type Set a = a -> Bool

empty :: Set a
empty _ = False

union :: Set a -> Set a -> Set a
union s1 s2 x = s1 x || s2 x

intersection :: Set a -> Set a -> Set a
intersection s1 s2 x = s1 x && s2 x

singleton :: Eq a => a -> Set a 
singleton a x = a == x 

insert :: Eq a => a -> Set a -> Set a
insert a s1 x = union (singleton a) s1 x





