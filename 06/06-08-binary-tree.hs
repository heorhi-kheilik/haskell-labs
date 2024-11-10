data BinaryTree a
    = BinaryTree a (BinaryTree a) (BinaryTree a)
    | NoTree
    deriving (Show)

treeLength NoTree = 0
treeLength (BinaryTree _ left right) = 1 + (treeLength left) + (treeLength right)

treeMap :: (a -> b) -> (BinaryTree a) -> (BinaryTree b)
treeMap _ NoTree = NoTree
treeMap mapFunc (BinaryTree value left right) = BinaryTree (mapFunc value) (treeMap mapFunc left) (treeMap mapFunc right)

treeTraverseD :: (b -> a -> b) -> b -> (BinaryTree a) -> b
treeTraverseD _ accValue NoTree = accValue
treeTraverseD accFunc accValue (BinaryTree value left right) =
    let leftAcc = accFunc (treeTraverseD accFunc accValue left) value in treeTraverseD accFunc leftAcc right

treeTraverseW :: (b -> a -> b) -> b -> (BinaryTree a) -> b
treeTraverseW _ accValue NoTree = accValue
treeTraverseW accFunc accValue tree = _treeTraverseWH [tree] accFunc accValue

_treeTraverseWH :: [(BinaryTree a)] -> (b -> a -> b) -> b -> b
_treeTraverseWH [] _ accValue = accValue
_treeTraverseWH (tree : trees) accFunc accValue = case tree
    of NoTree                       -> _treeTraverseWH trees accFunc accValue
       BinaryTree value left right  -> _treeTraverseWH newTrees accFunc newValue
        where newValue = accFunc accValue value
              newTrees = trees ++ [left, right]
