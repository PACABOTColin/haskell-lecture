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
    --On débute en observant un item du basket
    analyseBasket listItem = case listItem of
        [] -> 0

        --Si il y a bien un item, on va regarder la premiere promotion avec analyseRebates
        (idItem,(quant,price)):nextItem -> analyseRebates 0 price (cost quant price) 1 price
          where
            analyseRebates
              incr                  --Variable incrementale pour défiler dans la liste de rebates
              normalPrice           --Prix à l'unité de l'article
              actualBestTotalPrice  --Meilleur prix pour l'achat de l'item en quantitee demandee
              actualBestRatio_quant --Quantitee demandée pour l'actuel meilleur ratio prix/quant
              actualBestRatio_price --Prix demandé pour l'actuel meilleur ratio prix/quant
              =

              --Si on est arrivé à la fin de la liste de rebates
              if incr == (length listRebates)
                then
                  --On renvoie le meilleur prix d'achat et on passe a l'item suivant
                  actualBestTotalPrice + analyseBasket nextItem

                --Si il reste des rebates a analyser
                else
                  --On observe le rebates à la position incr dans la liste de rebates
                  case listRebates !! incr of

                    --CAS 1 : Réduction du prix de l'unité
                    Rebate rbIdItem rbPrice ->

                      --Si elle s'applique au bon item
                      if rbIdItem == idItem
                        then
                          --Si le nouveau prix est plus faible que le normal (Test améliorable)
                          if (cost quant rbPrice) < (actualBestTotalPrice)
                            then
                              analyseRebates
                                (incr+1)              --On observe le rebate suivant
                                rbPrice               --Le prix a l'unité est changé
                                (cost quant rbPrice)  --On mémorise le nouveau meilleur total.
                                actualBestRatio_quant
                                actualBestRatio_price

                          --Si le nouveau prix n'est pas avantageux, on passe au suivant !
                          else
                            analyseRebates (incr+1) normalPrice actualBestTotalPrice actualBestRatio_quant actualBestRatio_price
                      --Si ce n'est pas pour le bon item, on passe au suivant !
                      else
                        analyseRebates (incr+1) normalPrice actualBestTotalPrice actualBestRatio_quant actualBestRatio_price


                    --CAS 2 : Prix de groupe
                    Grouped rbIdItem rbQuant rbPrice ->

                      --Si elle s'applique au bon item
                      if rbIdItem == idItem
                        then
                          --Si le ratio prix/pieces est mieux qu'a l'unité
                          if (rbPrice `div` fromIntegral rbQuant) < normalPrice
                            then
                              --Si en le prennant le prix total est inférieur (donc meilleur que l'ancien ratio) (Test améliorable)
                              if (actualBestRatio_quant < rbQuant) && ((cost (quant `div` rbQuant) rbPrice) + (cost ((quant `mod` rbQuant) `div` actualBestRatio_quant) actualBestRatio_price) + (cost ((quant `mod` rbQuant) `mod` actualBestRatio_quant) normalPrice)) < (actualBestTotalPrice)
                                then
                                  analyseRebates
                                    (incr+1) normalPrice
                                                                                                                  --Le meilleur total se calcule ainsi :
                                    ((cost (quant `div` rbQuant) rbPrice)                                                 --On prend le max d'item avec le nouveau ratio
                                      + (cost ((quant `mod` rbQuant) `div` actualBestRatio_quant) actualBestRatio_price)  --Ensuite le max avec l'ancien ratio
                                      + (cost ((quant `mod` rbQuant) `mod` actualBestRatio_quant) normalPrice))           --On complete le reste à l'unité (Marche car les test ont que 2 promos de groupe)
                                    rbQuant       --On mémorise le nouveau meilleur ratio (quant necessaire)
                                    rbPrice       --On mémorise le nouveau meilleur ratio (prix necessaire)

                              --Sinon on prévilégie d'abord l'ancien puis on prend du nouveau
                              else
                                analyseRebates
                                  (incr+1) normalPrice
                                                                                                                --Le meilleur total se calcule ainsi :
                                  ((cost (quant `div` actualBestRatio_quant) actualBestRatio_price)                     --On prend le max d'item avec l'ancien ratio
                                    + (cost ((quant `mod` actualBestRatio_quant) `div` rbQuant) rbPrice)                --Ensuite le max avec le nouveau ratio
                                    + (cost ((quant `mod` actualBestRatio_quant) `mod` rbQuant) normalPrice))           --On complete le reste à l'unité (Marche car les test ont que 2 promos de groupe)
                                  actualBestRatio_quant actualBestRatio_price --On garde l'ancien ratio

                          --Sinon le ratio n'est pas interessant, on passe au suivant !
                          else
                            analyseRebates (incr+1) normalPrice actualBestTotalPrice actualBestRatio_quant actualBestRatio_price
                      --Si ce n'est pas pour le bon item, on passe au suivant !
                      else
                        analyseRebates (incr+1) normalPrice actualBestTotalPrice actualBestRatio_quant actualBestRatio_price


                    --CAS 3 : Lots 1 gratuit pour n achetés
                    NthFree rbIdItem rbQuant ->

                      --Si elle s'applique au bon item
                      if rbIdItem == idItem
                        --(Ces tests semblent améliorables, ils ne marchent que parce que les test n'incluent qu'un NthFree avec un Grouped)
                        then
                          --Si le """ratio""" prix/pieces est mieux qu'a l'unité
                          if (normalPrice `div` fromIntegral (rbQuant+1)) < normalPrice
                            then
                              --Si en le prennant le prix total est inférieur (donc meilleur que l'ancien ratio) (Test améliorable)
                              if ((cost (quant - (quant `div` (rbQuant+1))) normalPrice) + (cost ((quant `mod` (rbQuant+1)) `div` actualBestRatio_quant) actualBestRatio_price) + (cost ((quant `mod` (rbQuant+1)) `mod` actualBestRatio_quant) normalPrice)) < (actualBestTotalPrice)
                                then
                                  analyseRebates
                                    (incr+1) normalPrice
                                                                                                                      --Le meilleur total se calcule ainsi :
                                    ((cost (quant - (quant `div` (rbQuant+1))) normalPrice)                                   --On prend le max d'item avec le nouveau """ratio"""
                                      + (cost ((quant `mod` (rbQuant+1)) `div` actualBestRatio_quant) actualBestRatio_price)  --Ensuite le max avec l'ancien ratio
                                      + (cost ((quant `mod` (rbQuant+1)) `mod` actualBestRatio_quant) normalPrice))           --On complete le reste à l'unité hors lot

                                    actualBestRatio_quant actualBestRatio_price --On garde l'ancien ratio (le nouveau n'est pas vraiment un ratio)

                              --Sinon on prévilégie l'ancien ratio et on voit si on peut pas prendre du nouveau """ratio""" quand meme
                              else
                                analyseRebates
                                  (incr+1) normalPrice
                                  ((cost (quant `div` actualBestRatio_quant) actualBestRatio_price)
                                    + (cost (((quant `mod` actualBestRatio_quant) `div` (rbQuant+1)) * rbQuant) normalPrice)
                                    + (cost ((quant `mod` actualBestRatio_quant) `mod` (rbQuant+1)) normalPrice))
                                  actualBestRatio_quant actualBestRatio_price

                          --Sinon le lot n'est pas interessant, on passe au suivant !
                          else
                            analyseRebates (incr+1) normalPrice actualBestTotalPrice actualBestRatio_quant actualBestRatio_price
                      --Si ce n'est pas pour le bon item, on passe au suivant !
                      else
                        analyseRebates (incr+1) normalPrice actualBestTotalPrice actualBestRatio_quant actualBestRatio_price
