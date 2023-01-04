{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import GHC.Generics (Generic)

main :: IO ()
main = someFunc

--Data Types
data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a)
     deriving (Show, Eq, Generic, Ord)

data Set a = Empty | Set a (Set a)
      deriving (Read, Show, Eq, Generic, Ord)

-- Sections correspond with sections of the paper.
-- Section 2 - isBalanced Functionality

-- h - determines the height of the binary tree
h:: BinaryTree a -> Int
h (EmptyTree)    = 0
h (Node v l r) = (max (h l) (h r)) + 1

-- mh - determines the min height of a binary tree
mh:: BinaryTree a -> Int
mh (EmptyTree)    = 0
mh (Node v l r) = (min (mh l) (mh r)) + 1

-- balanced - determines if the binary tree is balanced
balanced :: BinaryTree a -> Bool
balanced (EmptyTree)  = True
balanced t@(Node v l r) = (((h t) - (mh t)) <= 1)

-- Section 3 - Arrays
-- lookup1 - looks up value of array via int index. Conceptual example: x = [1,2,3,4]; x[1] = 1
lookup1 :: BinaryTree a -> Int -> a
lookup1 (Node v l r) n = if (n == 1)
                         then v
                         else if (even n)
                         then lookup1 l (div n 2)
                         else lookup1 r (div n 2)


-- update1 - updates a value of array via int index. Conceptual example: x = [1,2,3,4]; x[1] = 10; x = [10,2,3,4]
update1 :: BinaryTree a -> Int -> a -> BinaryTree a
update1 (EmptyTree) n newVal = (Node newVal (EmptyTree) (EmptyTree))
update1 (Node v l r) n newVal = if (n == 1)
                                then (Node newVal l r)
                                else if (even n)
                                     then Node v (update1 l (div n 2) newVal) r
                                     else Node v l (update1 r (div n 2) newVal)


--adds - adds a haskell list of values to an existing array
adds :: BinaryTree a -> Int -> [a] -> BinaryTree a
adds t _ [] = t
adds t n (x:xs) = adds (update1 t (n+1) x) (n+1) xs

--list - Converts an array into list.
list :: BinaryTree a -> [a]
list (EmptyTree) = []
list (Node v l r) = v : (splice (list l) (list r))

--splice - Helper function of list. x = [x1, x2]; y = [y1,y2]; splice x y = [x1,y1,x2,y2]
splice :: [a] -> [a] -> [a]
splice [] ys = ys
splice (x:xs) ys = x : splice ys xs

-- array1 - takes in a list and converts it to an array with array tuple
array1 :: [a] -> BinaryTree a
array1 x =  (adds (EmptyTree) 0 x)


-- Functions in terms of an array tuple (length, array) for convenience
-- lookupA or lookup Array - similar to lookup1 above
lookupA :: (Int, BinaryTree a) -> Int -> a
lookupA (l, t) n = lookup1 t (n+1)

-- update - similar to update1 above with array tuple
update :: (Int, BinaryTree a) -> Int -> a -> (Int, BinaryTree a)
update (l, t) n newVal = (l, (update1 t (n+1) newVal))

-- len - returns length of array with array tuple
len :: (Int, BinaryTree a)  -> Int
len (l, t) = l

-- array - similar to array1 above with array tuple
array :: [a] -> (Int, BinaryTree a)
array x =  ((length(x)), (adds (EmptyTree) 0 x) )


-- Section 4 - Flexible Arrays

--del_hi - deletes head of the array
del_hi :: BinaryTree a -> Int -> BinaryTree a
del_hi (EmptyTree) n = (EmptyTree)
del_hi (Node v l r) n = if (n == 1)
                                then (EmptyTree)
                                else if (even n)
                                     then Node v (del_hi l (div n 2)) r
                                     else Node v l (del_hi r (div n 2))

--add_hi - adds element to the front of the array
add_hi :: BinaryTree a -> a -> BinaryTree a
add_hi t newVal = update1 t ((size_fast t)+1) newVal

--add_lo - adds element to the back of the array
add_lo :: BinaryTree a -> a -> BinaryTree a
add_lo (EmptyTree) x = (Node x (EmptyTree) (EmptyTree))
add_lo (Node a l r) x = (Node x (add_lo r a) l)

--del_lo - removes element from the back of the array
del_lo :: BinaryTree a -> BinaryTree a
del_lo (EmptyTree) =  (EmptyTree)
del_lo (Node v l r) = merge l r

--merge - merges two flexible_arrays together.
merge :: BinaryTree a -> BinaryTree a -> BinaryTree a
merge (EmptyTree) rr = rr
merge (Node v l r) rr = (Node v rr (merge l r))


-- Functions in terms of an array tuple (length, array) for convenience.
del_hiA :: (Int, BinaryTree a) -> (Int, BinaryTree a)
del_hiA (l, t) = (l-1, del_hi t l)

del_loA :: (Int, BinaryTree a) -> (Int, BinaryTree a)
del_loA (l, t) = (l-1, del_lo t)

add_hiA :: (Int, BinaryTree a) -> a -> (Int, BinaryTree a)
add_hiA (l, t) newVal = (l+1, update1 t (l+1) newVal)

add_loA :: (Int, BinaryTree a) -> a -> (Int, BinaryTree a)
add_loA (l, t) newVal = (l+1, add_lo t newVal)


-- Section 5 - Bigger Better Faster More!
-- (Extra part of the paper that makes some of the above functions faster and more efficient,
-- and adds some additional functions. This was not originally part of my project outline,
-- but I added it here for fun :)! )

-- size_fast - computes the size of a binary tree fast (O(log^2(t)))
size_fast:: BinaryTree a -> Int
size_fast (EmptyTree) = 0
size_fast (Node v l r) = let n = size_fast r in 1 + (2 * n) + (diff l n)

diff:: BinaryTree a -> Int -> Int
diff (EmptyTree) 0 = 0
diff (Node v l r) n = if n == 0
                      then 1
                      else if even n
                            then diff r ((div n 2) - 1)
                            else diff l (div n 2)

lh:: BinaryTree a -> Int
lh (EmptyTree) = 0
lh (Node v l r) = (lh l) + 1

-- 5.1 Initializing BraunTree with a Fixed Value
braun2_of:: a -> Int -> (BinaryTree a, BinaryTree a)
braun2_of x n = if n == 0
                then ((EmptyTree), (Node x (EmptyTree) (EmptyTree)))
                else let (s, t) = braun2_of x (div (n-1) 2)
                      in if odd n
                        then ((Node x s s), (Node x t s))
                        else ((Node x t s), (Node x t t))

braun_of:: a -> Int -> BinaryTree a
braun_of x n = fst (braun2_of x n)

-- 5.2 Converting a List into a Braun Tree
brauns:: Int -> [a] -> [BinaryTree a]
brauns k [] = []
brauns k xs = let ys = take (2^k) xs
                  zs = drop (2^k) xs
                  ts = brauns (k + 1) zs
                  in nodes ts ys (drop (2^k) ts)

nodes:: [BinaryTree a] -> [a] -> [BinaryTree a] -> [BinaryTree a]
nodes (l:ls) (x:xs) (r:rs) = (Node x l r) : (nodes ls xs rs)
nodes (l:ls) (x:xs) [] = (Node x l (EmptyTree)) : (nodes ls xs [])
nodes [] (x:xs) (r:rs) = (Node x (EmptyTree) r) : (nodes [] xs rs)
nodes [] (x:xs) [] = (Node x (EmptyTree) (EmptyTree)) : (nodes [] xs [])
nodes _ [] _ = []

brauns1 :: [a] -> BinaryTree a
brauns1 [] = (EmptyTree)
brauns1 xs = head (brauns 0 xs)

take_nths:: Int -> Int ->[a] -> [a]
take_nths _ _ [] = []
take_nths 0 k (x:xs) = x : take_nths ((2^k) - 1) k xs
take_nths i k (x:xs) = take_nths (i -1) k xs

braun_list:: Eq a => BinaryTree a -> [a] -> Bool
braun_list (EmptyTree) [] = True
braun_list (EmptyTree) xs = False
braun_list (Node v l r) xs = ((xs /= []) && (v == (head xs)) && (braun_list l (take_nths 1 1 xs)) && (braun_list r (take_nths 2 1 xs)))


t_brauns:: Int -> [a] -> Int
t_brauns k [] = 0
t_brauns k xs = let ys = take (2^k) xs
                    zs = drop (2^k) xs
                    ts = brauns (k+1) zs
                    in (4 * (min (2^k) (length(xs)))) + (t_brauns (k + 1) zs)


-- Section 7 - Priority Queues via Braun Trees
-- | /O(t*log t)/. Create a multiset from a list of elements.
-- uses an mset instead of a value a

--Sets:
-- Reference: https://codereview.stackexchange.com/questions/97896/implementation-of-set-data-type-in-haskell
-- fromList - converts a list into a set
fromList :: [a] -> Set a
fromList []     = Empty
fromList (x:xs) = Set x (fromList xs)

-- memeber - checks if an item is part of a set
member :: (Eq a) => a -> Set a -> Bool
member _ Empty                  = False
member a (Set x xs) | a == x    = True
                    | otherwise = member a xs

-- End of Reference

-- merge_sets - concatenates two sets
merge_sets:: Set a -> Set a -> Set a
merge_sets (Empty) b = b
merge_sets (Set a b) c = (Set a (merge_sets b c))

-- set_tree - converts a tree into a set
set_tree :: BinaryTree a -> Set a
set_tree (EmptyTree) = Empty
set_tree (Node a l r ) = Set a (merge_sets (set_tree l) (set_tree r))

-- traverse_set_LT - traverses the set to see if all of the elements are less than a value
traverse_set_LT::Ord a => a -> Set a -> Bool
traverse_set_LT m (Empty) = True
traverse_set_LT m (Set x b) = if m > x
                              then False
                              else traverse_set_LT m b

-- heap - determines if a binary tree is a heap or not.
heap:: Ord a => BinaryTree a -> Bool
heap (EmptyTree) = True
heap (Node v l r) = (heap l) && (heap r) && (traverse_set_LT v (merge_sets (set_tree l) (set_tree r)))

-- insert - inserts an element into a heap.
insert::Ord a => a -> BinaryTree a -> BinaryTree a
insert a (EmptyTree) = (Node a (EmptyTree) (EmptyTree))
insert a (Node x l r) = (if (a < x)
                        then (Node a (insert x r) l)
                        else (Node x (insert a r) l))

-- del_left - deletes the left item in a binarytree
del_left:: Ord a => BinaryTree a -> (a, BinaryTree a)
del_left (Node v (EmptyTree) r) = (v,r)
del_left (Node v l r) = (let (y, l') = del_left l in (y, (Node v r l')))

-- sift_down - sifts through the heap to ensure its correctness.
sift_down::Ord a => BinaryTree a -> a -> BinaryTree a -> BinaryTree a
sift_down (EmptyTree) a (EmptyTree) = (Node a (EmptyTree) (EmptyTree))
sift_down (Node x (EmptyTree) (EmptyTree)) a (EmptyTree) = (if a <= x
                                                            then (Node a (Node x (EmptyTree) (EmptyTree)) (EmptyTree))
                                                            else (Node x (Node a (EmptyTree) (EmptyTree)) (EmptyTree)))
sift_down n1@(Node x1 l1 r1) a n2@(Node x2 l2 r2) = (if ((a <= x1) && (a <= x2))
                                                      then (Node a n1 n2)
                                                      else if x1 <= x2
                                                          then (Node x1 (sift_down l1 a r1) n2)
                                                          else (Node x2 n1 (sift_down l2 a r2)))
-- del_min - deletes the minimum from the heap
del_min:: Ord a => BinaryTree a -> BinaryTree a
del_min (EmptyTree) = (EmptyTree)
del_min (Node x (EmptyTree) r1) = (EmptyTree)
del_min (Node x l r) = (let (y,l) = del_left l in (sift_down r y l))

le_root:: Ord a => a -> BinaryTree a -> Bool
le_root a (EmptyTree) = True
le_root a (Node x l r) =  (a <= x)

-- value - helper function to return the value of a node.
value:: BinaryTree a -> a
value (Node a l r) = a

-- replace_min - replaces the min item of the heap
replace_min:: Ord a => a -> BinaryTree a -> BinaryTree a
replace_min x (Node _ l r) = if ((le_root x l) && (le_root x r))
                             then (Node x l r)
                             else (let a = (value l) in (if (le_root a r)
                                                        then (Node a (replace_min x l) r)
                                                        else (Node (value r) l (replace_min x r))))

-- merge_pq - merges two priority queues or heaps together.
merge_pq:: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
merge_pq l (EmptyTree) = l
merge_pq n1@(Node a1 l1 r1) n2@(Node a2 l2 r2) = (if a1 <= a2
                                                  then (Node a1 n2 (merge_pq l1 r1))
                                                  else let (x,l) = del_left n1
                                                        in (Node a2 (replace_min x n2) l))

-- del_min2 - alternative, implementation of delete min above.
del_min2:: Ord a => BinaryTree a -> BinaryTree a
del_min2 (EmptyTree) = (EmptyTree)
del_min2 (Node _ l r) = merge_pq l r


-- heap_ofA - converts a list into a heap. Implementation A.
heap_ofA:: Ord a => [a] -> BinaryTree a
heap_ofA [] = (EmptyTree)
heap_ofA (a:as) = insert a (heap_ofA as)


-- heapify - alternative function for converting a list into a heap. Used by heap_ofB.
heapify:: Ord a => Int -> [a] -> (BinaryTree a, [a])
heapify 0 xs = ((EmptyTree), xs)
heapify n (x:xs) = (let (l,ys) = (heapify (n `div` 2) xs)
                        (r,zs) = (heapify ((n - 1) `div` 2) ys)
                        in  ((sift_down l x r), zs))

-- heap_ofB - converts a list into a heap. Implementation option B.
heap_ofB:: Ord a => [a] -> BinaryTree a
heap_ofB xs = fst (heapify (length xs) xs)

-- Section 8: Priority Sorting
merge_s8:: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
merge_s8 (EmptyTree) t2 = t2
merge_s8 t1 (EmptyTree) = t1
merge_s8 n1@(Node a1 l1 r1) n2@(Node a2 l2 r2) = (if a1 <= a2
                                                  then (Node a1 (merge_s8 l1 r1) n2)
                                                  else (Node a2 n1 (merge_s8 l2 r2)))
list_ofA:: Ord a => BinaryTree a -> [a]
list_ofA (EmptyTree) = []
list_ofA (Node a l r) = a : (list_ofA (merge_s8 l r))

list_ofB:: Ord a => BinaryTree a -> [a]
list_ofB (EmptyTree) = []
list_ofB n@(Node a l r) = a : (list_ofB (del_min2 n))

--From the above two fucntions for heap

sort_listA:: Ord a => [a] -> [a]
sort_listA [] = []
sort_listA xs = (list_ofA (heap_ofA xs))

sort_listB:: Ord a => [a] -> [a]
sort_listB [] = []
sort_listB xs = (list_ofB (heap_ofB xs))
