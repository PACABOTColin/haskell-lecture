module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import Data.List

import Student.Basics as B
import Student.Inventory
import qualified Student.Maps as SM

basics :: SpecWith ()
basics = describe "basics" $ do
  describe "simple operations" $ do
    prop "add" (\a b -> add a b == a + b)
    prop "divide" (\a b -> b == 0 || divide a b == a / b)
    prop "divide' /= 0" (\a b -> b == 0 || divide' a b == DoubleSuccess (a / b))
    prop "divide' == 0" (\a -> divide' a 0 == DoubleFailure)
    prop "divide'' /= 0" (\a b -> b == 0 || divide'' a b == B.Success (a / b))
    prop "divide'' == 0" (\a -> divide'' a 0 == B.Failure)
  describe "eval" $ do
    prop "Value" (\a -> eval (Value a) == a)
    prop "Add" (\a b -> eval (Add (Value a) (Value b)) == a + b)
    prop "Sub" (\a b -> eval (Sub (Value a) (Value b)) == a - b)
    prop "Mul" (\a b -> eval (Mul (Value a) (Value b)) == a * b)
    it "test 1" (eval (Add (Mul (Value 5) (Value 3.4)) (Value 8)) `shouldBe` 5*3.4+8)
  describe "eval'" $ do
    prop "Value" (\a -> eval' (Value a) == B.Success a)
    prop "Add" (\a b -> eval' (Add (Value a) (Value b)) == B.Success (a + b))
    prop "Sub" (\a b -> eval' (Sub (Value a) (Value b)) == B.Success (a - b))
    prop "Mul" (\a b -> eval' (Mul (Value a) (Value b)) == B.Success (a * b))
    it "test 1" (eval' (Add (Mul (Value 5) (Value 3.4)) (Value 8)) `shouldBe` B.Success (5*3.4+8))
    it "test 2" (eval' (Div (Div (Value 5) (Value 3.4)) (Value 0)) `shouldBe` B.Failure)
  describe "lists" $ do
    prop "listSum" (\l -> listSum (fromList l) == sum l)
    prop "listEq" (\l1 l2 -> listEq (fromList l1) (fromList l2) == ((l1 :: [Int]) == l2))
    prop "toList" (\l -> toList (fromList l) == (l :: [Int]))

fromList :: [a] -> B.List a
fromList = foldr Cons Empty

checkoutTests :: SpecWith ()
checkoutTests = describe "checkout" $ do
  describe "no rebates" $ do
    it "empty basket" $
      checkout sampleCatalog [] sampleInventory mempty `shouldBe` Right (sampleInventory, 0)
    it "out of stock" $
      checkout sampleCatalog [] sampleInventory (M.singleton 2 5) `shouldBe`
        Left OutOfStock
    it "buy all items" $
      checkout sampleCatalog [] sampleInventory (M.singleton 2 4) `shouldBe`
        Right (M.delete 2 sampleInventory, 4*200)
    it "something that has no description, but is in stock" $
      checkout sampleCatalog [] (M.insert 12 1 sampleInventory) (M.singleton 12 1) `shouldBe`
        Left DescriptionNotFound
    it "buy it all" $
      checkout sampleCatalog [] sampleInventory sampleInventory `shouldBe`
        Right (mempty, 12*785 + 4*200 + 8*24700 + 1*19700 + 43234*1500)
  describe "basic rebate" $ do
    let basket = M.fromList [(1, 3), (4, 1)]
        remaining = M.fromList
                      [ (1, 9)
                      , (2, 4)
                      , (3, 8)
                      , (5, 43234)
                      ]
    it "one item on sale" $
      checkout sampleCatalog [Rebate 1 700] sampleInventory basket `shouldBe`
        Right (remaining, 3*700 + 1*19700)
    it "two items on sale" $
      checkout sampleCatalog [Rebate 1 700, Rebate 4 5000] sampleInventory basket `shouldBe`
        Right (remaining, 3*700 + 1*5000)
    it "bad sale" $
      checkout sampleCatalog [Rebate 1 800] sampleInventory basket `shouldBe`
        Right (remaining, 3*785 + 1*19700)
    it "one good, and one bad sale" $
      checkout sampleCatalog [Rebate 1 700, Rebate 4 50000] sampleInventory basket `shouldBe`
        Right (remaining, 3*700 + 1*19700)
  describe "grouped rebate" $ do
    let basket = M.fromList [(1, 8)]
        remaining = M.fromList
                      [ (1, 4)
                      , (2, 4)
                      , (3, 8)
                      , (4, 1)
                      , (5, 43234)
                      ]
    it "simple grouped rebate" $
      checkout sampleCatalog [Grouped 1 8 1000] sampleInventory basket `shouldBe`
        Right (remaining, 1000)
    it "bad grouped rebate" $
      checkout sampleCatalog [Grouped 1 8 100000] sampleInventory basket `shouldBe`
        Right (remaining, 8*785)
    it "multi rebate 1" $
      checkout sampleCatalog [Grouped 1 4 1000] sampleInventory basket `shouldBe`
        Right (remaining, 2000)
    it "multi rebate 2" $
      checkout sampleCatalog [Grouped 1 5 3500, Grouped 1 3 2000] sampleInventory basket `shouldBe`
        Right (remaining, 3500 + 2000)
    it "mixed rebates" $
      checkout sampleCatalog [Grouped 1 5 6500, Grouped 1 3 2000] sampleInventory basket `shouldBe`
        Right (remaining, 2*785 + 4000)
  describe "nth free" $ do
    let basket = M.fromList [(1, 8)]
        remaining = M.fromList
                      [ (1, 4)
                      , (2, 4)
                      , (3, 8)
                      , (4, 1)
                      , (5, 43234)
                      ]
    it "simple nth" $
      checkout sampleCatalog [NthFree 1 7] sampleInventory basket `shouldBe`
        Right (remaining, 7*785)
  describe "LEVEL UP" $ do
    let basket = M.fromList [(1, 8)]
        remaining = M.fromList
                      [ (1, 4)
                      , (2, 4)
                      , (3, 8)
                      , (4, 1)
                      , (5, 43234)
                      ]
    it "bad" $
      checkout sampleCatalog [Grouped (-1) 6 1000, Grouped 133 2 500] sampleInventory basket `shouldBe`
        Right (remaining, 8*785)
    it "same product id, grouped" $
      checkout sampleCatalog [Grouped 1 6 1000, Grouped 1 2 500] sampleInventory basket `shouldBe`
        Right (remaining, 1500)
    it "same product id, mixed" $
      checkout sampleCatalog [Grouped 1 5 1000, NthFree 1 2] sampleInventory basket `shouldBe`
        Right (remaining, 1000 + 2*785)
    it "same product id, mixed, useless" $
      checkout sampleCatalog [Grouped 1 6 1000, NthFree 1 2] sampleInventory basket `shouldBe`
        Right (remaining, 1000 + 2*785)
    it "same product, size X" $
      checkout sampleCatalog (map (Rebate 1) [500..520]) sampleInventory basket `shouldBe`
        Right (remaining, 500*8)
    it "same product, size XL" $ do
      pendingWith "uncomment this if you feel confident"
      checkout sampleCatalog (map (Rebate 1) (reverse [500..600])) sampleInventory basket `shouldBe`
        Right (remaining, 500*8)
    it "same product, size XXL" $ do
      pendingWith "uncomment this if you feel confident"
      checkout sampleCatalog (map (Rebate 1) [500..1000]) sampleInventory basket `shouldBe`
        Right (remaining, 500*8)


mapTests :: Spec
mapTests = describe "MyMap tests" $ do
    it "empty" $ M.fromList SM.lempty `shouldBe` (M.empty :: M.Map Int String)
    prop "singleton" $ \k v -> SM.lsingleton k v == M.toList (M.singleton k v :: M.Map Int String)
    describe "insert" $
      prop "prop" $ \k v lst ->
        let mp = M.fromList lst :: M.Map Int String
            lst' = M.toList mp
        in  M.member k mp || M.fromList (SM.linsert k v lst') == M.insert k v mp
    describe "delete" $
      prop "prop" $ \k lst ->
        let mp = M.fromList lst :: M.Map Int String
            lst' = M.toList mp
        in  M.fromList (SM.ldelete k lst') == M.delete k mp
    describe "lookup" $
      prop "prop" $ \k lst ->
        let mp = M.fromList lst :: M.Map Int String
            lst' = M.toList mp
        in  M.lookup k mp == SM.llookup k lst'
    describe "lmaximum" $
      prop "prop" $ \lst ->
        let mp = M.fromList lst :: M.Map Int String
            lst' = M.toList mp
        in  SM.lmaximum lst' == case lst' of
              [] -> Nothing
              _ -> Just (maximum (map snd lst'))
    describe "fromList" $
      prop "prop" $ \lst ->
        let mp = M.fromList lst :: M.Map Int String
            lst' = M.toList mp
        in  lst' == sort (SM.fromList lst)
    describe "lsum" $
      prop "prop" $ \lst ->
        let mp = M.fromList lst :: M.Map String Integer
            lst' = M.toList mp
        in  SM.lsum lst' == sum mp
    describe "lminimum" $
      prop "prop" $ \lst ->
        let mp = M.fromList lst :: M.Map Int String
            lst' = M.toList mp
        in  SM.lminimum lst' == case lst' of
              [] -> Nothing
              _ -> Just (minimum (map snd lst'))
    describe "lfold" $ do
      prop "prop add" $ \lst ->
        let mp = M.fromList lst :: M.Map String Int
            lst' = M.toList mp
        in  SM.lfold (+) 0 lst' == sum (map snd lst')
      prop "prop multiply" $ \lst ->
        let mp = M.fromList lst :: M.Map String Int
            lst' = M.toList mp
        in  SM.lfold (*) 1 lst' == product (map snd lst')
    -- describe "intersectionWith" $
    --   prop "prop" $ \lst1 lst2 ->
    --     let mp1 = M.fromList lst1 :: M.Map Int String
    --         lst1' = M.toList mp1
    --         mp2 = M.fromList lst2 :: M.Map Int Int
    --         lst2' = M.toList mp2
    --     in  M.fromList (SM.intersectionWith replicate lst2' lst1') ==
    --         M.merge M.dropMissing M.dropMissing (M.zipWithMatched (const replicate)) mp2 mp1


main :: IO ()
main = hspec $ do
  mapTests
  basics
  checkoutTests
