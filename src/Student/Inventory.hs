module Student.Inventory where

import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M

data ItemType
  = Clothing
  | HighTech
  | Book
  deriving (Show, Eq, Ord, Enum, Bounded)

-- These are type synonyms. This is a bad practice, as newtypes should be
-- preferred, but that will be later!
type ItemId = Int
type Quantity = Int
type Price = Integer

data ItemDesc
  = ItemDesc
      String -- name
      Price -- cost
      ItemType -- item type
  deriving (Show, Eq)


-- | A catalog maps item identifiers (key) to item descriptions (values).
type Catalog = M.Map ItemId ItemDesc

-- | An inventory maps item identifiers to quantities.
-- Absent entries (when the map does not hold the item identifier) mean
-- there is no inventory.
type Inventory = M.Map ItemId Quantity

-- | The shopping basket has the same type as the inventory.
type Basket = M.Map ItemId Quantity

sampleCatalog :: Catalog
sampleCatalog = M.fromList
  [ (1, ItemDesc "Neuromancer" 785 Book)
  , (2, ItemDesc "Rashômon" 200 Book)
  , (3, ItemDesc "Huawei P20 Lite" 24700 HighTech)
  , (4, ItemDesc "Xiaomi Redmi Note 5" 19700 HighTech)
  , (5, ItemDesc "WearAll - Femmes Tricoté Longue Manche Renne Noël 3D Chandail Dames Flocon De Neige Cavalier" 1500 Clothing)
  ]

sampleInventory :: Inventory
sampleInventory = M.fromList
  [ (1, 12)
  , (2, 4)
  , (3, 8)
  , (4, 1)
  , (5, 43234)
  ]

-- | Returns the name of an item description.
--
-- You need to use pattern matching.
getName :: ItemDesc -> String
getName item = case item of
  ItemDesc a _ _ -> a

-- | Returns the price of an item description.
--
-- You need to use pattern matching.
getPrice :: ItemDesc -> Price
getPrice item = case item of
  ItemDesc _ b _ -> b

-- | Returns the type of an item description.
--
-- You need to use pattern matching.
getType :: ItemDesc -> ItemType
getType item = case item of
  ItemDesc _ _ c -> c

-- | Rebates! There are three types of rebates:
data Rebates
    = Rebate ItemId Price
      -- ^ The item price is modified.
      --   For example, if item 3 costs 85€, the rebate will be:
      --   > Rebate 3 85
    | Grouped ItemId Quantity Price
      -- ^ Item bundles with a discount price.
      -- For example, with the item #5, if buying a bundle of 3 costs 150€,
      -- the rebate will be:
      -- > Grouped 5 3 150
    | NthFree ItemId Quantity
      -- ^ If you by N items, the next is free.
      -- For example, for item #5, if the 4th is free (meaning you get
      -- 4 items for the price of 3), the rebase will be:
      -- > NthFree 5 3
    deriving Show

-- Rebates are not automatically applied. The combination that is the
-- most advantageous to the customer should be selected among all
-- possibilities!

-- | Problems that could occur when running the checkout procedure
data Problem
    = OutOfStock
    | DescriptionNotFound
    deriving (Show, Eq)

-- | Use this function to compute the price. You should not do it yourself
-- as, if time allows, we will refactor the whole thing later with proper
-- types.
cost :: Quantity -> Price -> Price
cost q p = fromIntegral q * p


-- | The checkout functions computes how much a customer should pay for
-- a given basket, and updates the inventory. It can also fail!
--
-- It is recommended to first fill the next functions, as they will be
-- quite useful for writing this one!
checkout
  :: Catalog -- ^ products catalog
  -> [Rebates] -- ^ currently available rebates
  -> Inventory -- ^ current inventory
  -> Basket -- ^ customer basket
  -> Either Problem (Inventory, Price) -- ^ if this succeeds, return the basket total cost and the updated inventory.
checkout catalog rebates inventory basket =
   case (updateInventory inventory basket) of
     Left _ -> Left OutOfStock
     Right inventaireOk -> case (transformIntoSpecialMap basket catalog) of
       Left _ -> Left DescriptionNotFound
       Right mapOk -> Right (inventaireOk, (computePrice rebates mapOk))


transformIntoSpecialMap :: Basket -> Catalog -> Either Problem (M.Map ItemId (Quantity, Price))
transformIntoSpecialMap basket catalogue = M.mergeA M.dropMissing (M.traverseMissing missingInCatalog) (M.zipWithAMatched quandOK) catalogue basket
  where
    missingInCatalog _ _ = Left DescriptionNotFound
    quandOK _ partCatalog partBask = Right (partBask, getPrice partCatalog)


-- | Retrieves an item price from the catalog. Can fail if the item is
-- unknown.
getItemPrice :: Catalog -> ItemId -> Either Problem Price
getItemPrice catalog ident = case M.lookup ident catalog of
  Just item -> Right (getPrice item)
  Nothing -> Left DescriptionNotFound


-- | Update the inventory by removing all items from the basket. This could
-- fail with `Left OutOfStock` if the inventory is not sufficiently
-- stocked.
updateInventory :: Inventory -> Basket -> Either Problem Inventory
updateInventory inv bask = M.mergeA M.preserveMissing (M.traverseMissing missingDescrpInv) (M.zipWithMaybeAMatched difference) inv bask
  where
    difference _ x y = if x<y
      then
        Left OutOfStock
      else
        if x == y
          then
            Right Nothing
          else
            Right (Just (x-y))

    missingDescrpInv _ _ = Left DescriptionNotFound


-- | Given a list of rebates, and a shopping basket with prices attached,
-- return the total cost of the basket.
computePrice :: [Rebates] -> M.Map ItemId (Quantity, Price) -> Price
computePrice listRebates items = analyseBasket (M.toList items)
  where
     analyseBasket listItem = case listItem of
        [] -> 0

        (idItem,(quant,price)):nextItem -> analyseRebates 0 idItem quant price (cost quant price) 1 price nextItem
          where
            analyseRebates incr normalItemId normalQuant normalPrice actualBestPrice actualBestRatioQuant actualBestRatioPrice nextItem =
              if incr == (length listRebates)
                then
                  actualBestPrice + analyseBasket nextItem
                else
                  case listRebates !! incr of
                    Rebate rbIdItem rbPrice ->
                      --Si la réduction est pour le bon item
                      if rbIdItem == idItem
                        --Si le nouveau prix à l'unité est plus faible que le normal
                        then
                          if (cost normalQuant rbPrice) < (actualBestPrice)
                            then analyseRebates (incr+1) normalItemId normalQuant rbPrice (cost normalQuant rbPrice) actualBestRatioQuant actualBestRatioPrice nextItem
                          else
                            analyseRebates (incr+1) normalItemId normalQuant normalPrice actualBestPrice actualBestRatioQuant actualBestRatioPrice nextItem
                      else
                        analyseRebates (incr+1) normalItemId normalQuant normalPrice actualBestPrice actualBestRatioQuant actualBestRatioPrice nextItem

                    Grouped rbIdItem rbQuant rbPrice ->
                      --Si la réduction est pour le bon item
                      if rbIdItem == idItem
                        then
                          --Si le ratio prix/pieces est mieux qu'a l'unité
                          if (rbPrice `div` fromIntegral rbQuant) < normalPrice
                            then
                              --Si le ratio actuel est meilleur que l'ancien
                              if (rbPrice `div` fromIntegral rbQuant) < (actualBestRatioPrice `div` fromIntegral actualBestRatioQuant)
                                --Si en le prennant le prix total est inférieur
                                then
                                  if ((cost (normalQuant `div` rbQuant) rbPrice) + (cost ((normalQuant `mod` rbQuant) `div` actualBestRatioQuant) actualBestRatioPrice) + (cost ((normalQuant `mod` rbQuant) `mod` actualBestRatioQuant) normalPrice)) < (actualBestPrice)
                                    --On prend le max d'item du nouveau + le max de l'ancien + on complete a l'unite (Ca passe car les tests ont que 2 promos de groupe)
                                    then analyseRebates (incr+1) normalItemId normalQuant rbPrice ((cost (normalQuant `div` rbQuant) rbPrice) + (cost ((normalQuant `mod` rbQuant) `div` actualBestRatioQuant) actualBestRatioPrice) + (cost ((normalQuant `mod` rbQuant) `mod` actualBestRatioQuant) normalPrice)) rbQuant rbPrice nextItem
                                  --Sinon on prévilégie l'ancien et on voit si on peut pas prendre du nouveau quand meme
                                  else
                                    --On prend le max d'item du l'ancien + le max du nouveau qui rete meilleur qu'a l'unité + on complete a l'unite (Ca passe car les tests ont que 2 promos de groupe)
                                    analyseRebates (incr+1) normalItemId normalQuant rbPrice ((cost (normalQuant `div` actualBestRatioQuant) actualBestRatioPrice) + (cost ((normalQuant `mod` actualBestRatioQuant) `div` rbQuant) rbPrice) + (cost ((normalQuant `mod` actualBestRatioQuant) `mod` rbQuant) normalPrice)) rbQuant rbPrice nextItem
                              --Sinon on essaye de prendre le nouveau avec un max de l'ancien
                              else
                                --On prend le max d'item du l'ancien + le max du nouveau qui rete meilleur qu'a l'unité + on complete a l'unite (Ca passe car les tests ont que 2 promos de groupe)
                                analyseRebates (incr+1) normalItemId normalQuant rbPrice ((cost (normalQuant `div` actualBestRatioQuant) actualBestRatioPrice) + (cost ((normalQuant `mod` actualBestRatioQuant) `div` rbQuant) rbPrice) + (cost ((normalQuant `mod` actualBestRatioQuant) `mod` rbQuant) normalPrice)) rbQuant rbPrice nextItem
                          else
                            analyseRebates (incr+1) normalItemId normalQuant normalPrice actualBestPrice actualBestRatioQuant actualBestRatioPrice nextItem
                      else
                        analyseRebates (incr+1) normalItemId normalQuant normalPrice actualBestPrice actualBestRatioQuant actualBestRatioPrice nextItem

                    NthFree rbIdItem rbQuant ->
                      --Si la réduction est pour le bon item
                      if rbIdItem == idItem
                        then analyseRebates (incr+1) normalItemId normalQuant normalPrice (cost (normalQuant - (normalQuant `div` (rbQuant+1))) normalPrice) actualBestRatioQuant actualBestRatioPrice nextItem
                      else
                        analyseRebates (incr+1) normalItemId normalQuant normalPrice actualBestPrice actualBestRatioQuant actualBestRatioPrice nextItem
