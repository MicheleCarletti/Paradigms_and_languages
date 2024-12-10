-- Binary Search Tree (BST)

data Tree a = EmptyTree
    | Node a (Tree a) (Tree a)
    deriving (Show, Read, Eq)

-- Tree with a single leaf
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- Insert an element in the BST
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node a left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

-- Check if an element is part of a BST
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

