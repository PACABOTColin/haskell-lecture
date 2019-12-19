module Student.Maps where

-- | Maps are lists of key/value pairs
--
-- !IMPORTANT! keys must be kept unique!
type MyMap k v = [(k, v)]

-- | The empty map
lempty :: MyMap k v
lempty = []

-- | The map with a single key/value pair
lsingleton :: k -> v -> MyMap k v
lsingleton k v = [(k, v)]

-- | Insert a key value pair into the map.
-- On key collision, the value is overwritten.
linsert
  :: Eq k
  => k
  -> v
  -> MyMap k v
  -> MyMap k v
linsert k v list = case list of
  [] -> lsingleton k v
  (k2, v2):list2 -> if k2 == k
  then
    (k, v): list2
  else
    (k2, v2): (linsert k v list2)


-- | Remove a key/value pair based on a key.
ldelete
  :: Eq k
  => k
  -> MyMap k v
  -> MyMap k v
ldelete k list = case list of
  [] -> []
  (k2, v2):list2 -> if k2 == k
  then
    list2
  else
    (k2, v2): (ldelete k list2)

-- | Finds the value associated with a key, if it exists.
llookup
  :: Eq k
  => k
  -> MyMap k v
  -> Maybe v
llookup k list = case list of
  [] -> Nothing
  (k2, v2):list2 -> if k2 == k
  then
    Just v2
  else
    llookup k list2

-- | Finds the largest value, if it exists.
lmaximum
  :: Ord v
  => MyMap k v
  -> Maybe v
lmaximum list = case list of
  [] -> Nothing
  (k, v): list -> case lmaximum list of
    Nothing -> Just v
    Just v2 -> if v > v2 
      then 
        Just v 
      else 
        Just v2

-- | Create a Map from a list. Althrough they have the same type,
-- the "MyMap" must preserve the unicity key invariant. Keys that appear
-- earlier in the input list are overwritten on collision.
fromList
  :: Eq k
  => [(k, v)]
  -> MyMap k v
-- fromList = undefined
fromList list = go lempty list
  where
    go curmap lst =
      case lst of
        [] -> curmap
        (k, v) : lst2 -> go (linsert k v curmap) lst2
-- | Do you understand what this function is? Even if you do not, can you
-- write it?
lfold :: (v -> a -> a)
      -> a
      -> MyMap k v
      -> a
lfold func a list = case list of
  [] -> a
  (k, v): list2 -> func v (lfold func a list2)

-- | Sum of all the values in the map. Implement it with lfold.
lsum
  :: MyMap k Integer
  -> Integer
lsum map = lfold (+) 0 map 


-- | Finds the smallest value, if it exists. Implement it with lfold.
lminimum
  :: Ord v
  => MyMap k v
  -> Maybe v
lminimum map = lfold mini Nothing map
  where
    mini v mv = case mv of
      Nothing -> Just v
      Just v' -> Just (min v v')

-- | Intersection of two maps. You should use lfold.
intersectionWith
  :: Eq k
  => (a -> b -> c)
  -> MyMap k a
  -> MyMap k b
  -> MyMap k c
intersectionWith = undefined

