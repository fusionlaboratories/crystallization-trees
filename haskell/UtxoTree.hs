module UtxoTree (UtxoTree(..), Transaction(..), insertUtxo, deleteUtxo, searchUtxo, modifyWalletBalance, printUtxoTree)
where

import BiTree (BinaryTree(..), Wallet(..), insert, search, modify)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

data UtxoTree = EmptyUtxo
              | NodeUtxo UUID Transaction Int UtxoTree UtxoTree deriving (Show)

data Transaction = Transaction String Double deriving (Eq, Show)

instance Eq UtxoTree where
    EmptyUtxo == EmptyUtxo = True
    (NodeUtxo _ _ _ _ _) == EmptyUtxo = False
    EmptyUtxo == (NodeUtxo _ _ _ _ _) = False
    (NodeUtxo tag1 tx1 _ left1 right1) == (NodeUtxo tag2 tx2 _ left2 right2) =
        tag1 == tag2 && tx1 == tx2 && left1 == left2 && right1 == right2

instance Ord UtxoTree where
    (NodeUtxo _ (Transaction _ amt1) _ _ _) `compare` (NodeUtxo _ (Transaction _ amt2) _ _ _) = amt1 `compare` amt2

heightUtxo :: UtxoTree -> Int
heightUtxo EmptyUtxo = 0
heightUtxo (NodeUtxo _ _ h _ _) = h

balanceFactorUtxo :: UtxoTree -> Int
balanceFactorUtxo EmptyUtxo = 0
balanceFactorUtxo (NodeUtxo _ _ _ left right) = heightUtxo left - heightUtxo right

updateHeightUtxo :: UtxoTree -> UtxoTree
updateHeightUtxo EmptyUtxo = EmptyUtxo
updateHeightUtxo (NodeUtxo tag tx _ left right) =
    NodeUtxo tag tx (1 + max (heightUtxo left) (heightUtxo right)) left right

rotateLeftUtxo :: UtxoTree -> UtxoTree
rotateLeftUtxo EmptyUtxo = EmptyUtxo
rotateLeftUtxo (NodeUtxo tag1 tx1 _ left1 (NodeUtxo tag2 tx2 _ left2 right2)) =
    updateHeightUtxo $ NodeUtxo tag2 tx2 h2 (updateHeightUtxo $ NodeUtxo tag1 tx1 h1 left1 left2) right2
    where h1 = heightUtxo left1
          h2 = heightUtxo right2
rotateLeftUtxo node@(NodeUtxo _ _ _ EmptyUtxo _) = node

rotateRightUtxo :: UtxoTree -> UtxoTree
rotateRightUtxo EmptyUtxo = EmptyUtxo
rotateRightUtxo (NodeUtxo tag1 tx1 _ (NodeUtxo tag2 tx2 _ left2 right2) right1) =
    updateHeightUtxo $ NodeUtxo tag2 tx2 h2 left2 (updateHeightUtxo $ NodeUtxo tag1 tx1 h1 right2 right1)
    where h1 = heightUtxo right1
          h2 = heightUtxo left2
rotateRightUtxo node@(NodeUtxo _ _ _ EmptyUtxo _) = node

balanceUtxo :: UtxoTree -> UtxoTree
balanceUtxo tree@(NodeUtxo tag tx _ left right)
    | balanceFactorUtxo tree > 1 = if balanceFactorUtxo left < 0 then rotateRightUtxo $ rotateLeftUtxo tree else rotateRightUtxo tree
    | balanceFactorUtxo tree < -1 = if balanceFactorUtxo right > 0 then rotateLeftUtxo $ rotateRightUtxo tree else rotateLeftUtxo tree
    | otherwise = tree
balanceUtxo tree = tree

insertUtxo :: Transaction -> UtxoTree -> IO UtxoTree
insertUtxo tx EmptyUtxo = do
    tag <- nextRandom
    return $ NodeUtxo tag tx 1 EmptyUtxo EmptyUtxo
insertUtxo tx@(Transaction id amt) (NodeUtxo tag' tx'@(Transaction id' amt') height left right) = do
    if id == id' then
        return $ NodeUtxo tag' tx height left right
    else
        if amt <= amt' then do
            newLeft <- insertUtxo tx left
            return $ balanceUtxo $ NodeUtxo tag' tx' (height + 1) newLeft right
        else do
            newRight <- insertUtxo tx right
            return $ balanceUtxo $ NodeUtxo tag' tx' (height + 1) left newRight

searchUtxo :: String -> UtxoTree -> Maybe Transaction
searchUtxo _ EmptyUtxo = Nothing
searchUtxo name (NodeUtxo _ tx@(Transaction txName _) _ left right)
    | name == txName = Just tx
    | name < txName = searchUtxo name left
    | otherwise = searchUtxo name right

deleteUtxo :: UUID -> UtxoTree -> UtxoTree
deleteUtxo _ EmptyUtxo = EmptyUtxo
deleteUtxo utxoId node@(NodeUtxo nodeId tx height left right)
    | utxoId == nodeId = mergeUtxo left right
    | utxoId < nodeId = balanceUtxo $ NodeUtxo nodeId tx (heightUtxo left - 1) (deleteUtxo utxoId left) right
    | otherwise = balanceUtxo $ NodeUtxo nodeId tx (heightUtxo right - 1) left (deleteUtxo utxoId right)
    where mergeUtxo EmptyUtxo right = right
          mergeUtxo left EmptyUtxo = left
          mergeUtxo left right = let (minNode, minId) = findMinNodeUtxo right
                                 in NodeUtxo minId minNode (heightUtxo left - 1) left (deleteUtxo minId right)
          findMinNodeUtxo (NodeUtxo nodeId tx _ EmptyUtxo _) = (tx, nodeId)
          findMinNodeUtxo (NodeUtxo nodeId tx _ left _) = findMinNodeUtxo left


modifyWalletBalance :: String -> Double -> BinaryTree -> UtxoTree -> IO (BinaryTree, UtxoTree)
modifyWalletBalance _ _ walletTree EmptyUtxo = return (walletTree, EmptyUtxo)
modifyWalletBalance walletName delta walletTree (NodeUtxo utxoId tx@(Transaction txName txAmount) _ left right)
    | walletName == txName = do
        let newWalletTree = case modify walletName delta walletTree of
                              Just tree -> tree
                              Nothing   -> walletTree
        return (newWalletTree, deleteUtxo utxoId (NodeUtxo utxoId tx (heightUtxo left + 1) left right))
    | walletName < txName = do
        (newWalletTree, newLeft) <- modifyWalletBalance walletName delta walletTree left
        let newUtxoTree = NodeUtxo utxoId tx (heightUtxo newLeft + 1) newLeft right
        return (newWalletTree, balanceUtxo newUtxoTree)
    | otherwise = do
        (newWalletTree, newRight) <- modifyWalletBalance walletName delta walletTree right
        let newUtxoTree = NodeUtxo utxoId tx (heightUtxo newRight + 1) left newRight
        return (newWalletTree, balanceUtxo newUtxoTree)


printUtxoTree :: UtxoTree -> Int -> String
printUtxoTree EmptyUtxo _ = ""
printUtxoTree (NodeUtxo utxoId tx height left right) level =
  printUtxoTree right (level + 1) ++
  replicate level ' ' ++ "|--" ++
  show tx ++ ": " ++ "UTXO ID=" ++ show utxoId ++ ", height=" ++ show height ++ "\n" ++
  printUtxoTree left (level + 1)



