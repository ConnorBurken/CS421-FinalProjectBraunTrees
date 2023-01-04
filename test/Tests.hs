module Tests where
import Data.List ((\\))

import Main


empty_tree = (EmptyTree)
balanced_tree1 = (Node 1 (Node 4 (Node 6 (Node 0 EmptyTree EmptyTree) EmptyTree) (Node 8 EmptyTree EmptyTree)) (Node 5 (Node 7 EmptyTree EmptyTree) (Node 9 EmptyTree EmptyTree)))
balanced_tree2 = (Node 1 (Node 2 (Node 4 (Node 8 EmptyTree EmptyTree) EmptyTree) (Node 6 EmptyTree EmptyTree)) (Node 3 (Node 5 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree)))
unbalanced_tree1 = (Node 1 EmptyTree (Node 5 (Node 7 EmptyTree EmptyTree) (Node 9 (Node 16 (Node 33 EmptyTree EmptyTree) EmptyTree) EmptyTree)))
unbalanced_tree2 =  (Node 1 (Node 5 (Node 7 EmptyTree EmptyTree) (Node 9 (Node 16 (Node 33 EmptyTree EmptyTree) EmptyTree) EmptyTree)) EmptyTree)

l1 = [9,3,7,2]
l2 = [44,2,34,7,9,10]
l3 = [10,1,2,3,4,5]
l4 = [1,1,1,1,1,1,1]

--Lists above as an array
la1 = (Node 9 (Node 3 (Node 2 EmptyTree EmptyTree) EmptyTree) (Node 7 EmptyTree EmptyTree))
la2 = (Node 44 (Node 2 (Node 7 EmptyTree EmptyTree) (Node 10 EmptyTree EmptyTree)) (Node 34 (Node 9 EmptyTree EmptyTree) EmptyTree))
la3 = (Node 10 (Node 1 (Node 3 EmptyTree EmptyTree) (Node 5 EmptyTree EmptyTree)) (Node 2 (Node 4 EmptyTree EmptyTree) EmptyTree))
la4 = (Node 1 (Node 1 (Node 1 EmptyTree EmptyTree) (Node 1 EmptyTree EmptyTree)) (Node 1 (Node 1 EmptyTree EmptyTree) (Node 1 EmptyTree EmptyTree)))

true_heap = (Node 2 (Node 3 (Node 7 EmptyTree EmptyTree) EmptyTree) (Node 9 EmptyTree EmptyTree))
non_heap = la1

allTests :: [([Bool], String)]
allTests = [ (braun_trees, "Section 2 - Braun Tree tests"),
            (arrays, "Section 3 - Array tests"),
            (flexible_arrays, "Section 4 - Flexible Array tests"),
            --(faster_implementations, "Section 5 - Faster Implementations tests"),
             (priority_queue, "Section 7 - Priority Queue tests"),
            (priority_queue_sorting, "Section 8 - Sorting with Priority Queue tests")
               ]


    --- Problems
    --- ========

    --- Manual Translation
    --- ------------------

    --- ### `factk :: Integer -> (Integer -> t) -> t`


braun_trees :: [Bool]
braun_trees = [ (h empty_tree) == 0,
                (h balanced_tree1) == 4,
                (h balanced_tree2) == 4,
                (h unbalanced_tree1) == 5,
                (h unbalanced_tree2) == 5,
                (mh empty_tree) == 0,
                (mh balanced_tree1) == 3,
                (mh balanced_tree2) == 3,
                (mh unbalanced_tree1) == 1,
                (mh unbalanced_tree2) == 1,
                (balanced empty_tree == True),
                (balanced balanced_tree1 == True),
                (balanced balanced_tree2 == True),
                (balanced unbalanced_tree1 == False),
                (balanced unbalanced_tree2 == False)
                ]

arrays :: [Bool]
arrays = [ ((test_lookup1 l1 la1) == True),
           ((test_lookup1 l2 la2) == True),
           ((test_lookup1 l3 la3) == True),
           ((test_lookup1 l4 la4) == True),
           ((array1 l1) == la1),
           ((array1 l2) == la2),
           ((array1 l3) == la3),
           ((array1 l4) == la4),
           ((update1 la1 1 444) == (Node 444 (Node 3 (Node 2 EmptyTree EmptyTree) EmptyTree) (Node 7 EmptyTree EmptyTree))),
           ((update1 la1 10 444) == (Node 9 (Node 3 (Node 2 EmptyTree EmptyTree) (Node 444 EmptyTree EmptyTree)) (Node 7 EmptyTree EmptyTree))),
           ((adds la1 (size_fast la1) [10,11,12,13,14]) == (array1 (l1 ++ [10,11,12,13,14]))),
           ((adds la2 (size_fast la2) [100,11,102,130,114]) == (array1 (l2 ++ [100,11,102,130,114]))),
           (list (adds la1 (size_fast la1) [10,11,12,13,14]) == (l1 ++ [10,11,12,13,14])),
           (list (adds la2 (size_fast la2) [100,11,102,130,114]) == (l2 ++ [100,11,102,130,114]))
            ]

flexible_arrays :: [Bool]
flexible_arrays = [ ((list (add_lo (array1 l1) 10)) == [10,9,3,7,2]),
                    ((list (add_hi (array1 l1) 10)) == [9,3,7,2,10]),
                    ((list (del_lo (array1 l1))) == [3,7,2]),
                    ((list (del_hi (array1 l1) ((length l1)))) == [9,3,7]),
                    ((list (merge (array1 l1) (array1 l2))) == [9,44,3,2,7,34,2,7,9,10])
                        ]


priority_queue :: [Bool]
priority_queue = [((heap true_heap) == True),
                  ((heap non_heap) == False),
                  ((insert 4 true_heap) == (Node 2 (Node 4 (Node 9 EmptyTree EmptyTree) EmptyTree) (Node 3 (Node 7 EmptyTree EmptyTree) EmptyTree))),
                  ((del_left true_heap)== (7,Node 2 (Node 9 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree))),
                  ((heap_ofA l1) == (Node 2 (Node 7 (Node 9 EmptyTree EmptyTree) EmptyTree) (Node 3 EmptyTree EmptyTree))),
                  ((heap_ofB l2) == (Node 2 (Node 7 (Node 34 EmptyTree EmptyTree) (Node 44 EmptyTree EmptyTree)) (Node 9 (Node 10 EmptyTree EmptyTree) EmptyTree)))
                    ]

priority_queue_sorting :: [Bool]
priority_queue_sorting = [((sort_listA l2) == [2,7,9,10,34,44]),
                          ((sort_listB l2) == [2,7,9,10,34,44])]


--References the tree and checks against a list for position
test_lookup1:: Eq a => [a] -> BinaryTree a -> Bool
test_lookup1 x t = aux x t 1
                        where
                          aux [] t n = True
                          aux (x:xs) t n = if x /= (lookup1 t n)
                                           then False
                                           else aux xs t (n+1)
