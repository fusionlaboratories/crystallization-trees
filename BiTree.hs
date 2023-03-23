module BiTree (BinaryTree(..), Wallet(..), insert, delete, findMin, findMax, printTree, search, modify)
where

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Control.Applicative ((<|>))

data BinaryTree = Empty
                | Node UUID Wallet Double Int BinaryTree BinaryTree deriving (Show)


instance Eq BinaryTree where
    Empty == Empty = True
    (Node _ _ _ _ _ _) == Empty = False
    Empty == (Node _ _ _ _ _ _) = False
    (Node tag1 wallet1 _ _ left1 right1) == (Node tag2 wallet2 _ _ left2 right2) =
        tag1 == tag2 && wallet1 == wallet2 && left1 == left2 && right1 == right2

instance Ord BinaryTree where
    (Node _ _ amt1 _ _ _) `compare` (Node _ _ amt2 _ _ _) = amt1 `compare` amt2

data Wallet = Wallet String Double deriving (Eq, Show)

height :: BinaryTree -> Int
height Empty = 0
height (Node _ _ _ h _ _) = h

balanceFactor :: BinaryTree -> Int
balanceFactor Empty = 0
balanceFactor (Node _ _ _ _ left right) = height left - height right

updateHeight :: BinaryTree -> BinaryTree
updateHeight Empty = Empty
updateHeight (Node tag wallet total _ left right) =
    Node tag wallet total (1 + max (height left) (height right)) left right

rotateLeft :: BinaryTree -> BinaryTree
rotateLeft Empty = Empty
rotateLeft (Node tag1 wallet1 total1 _ left1 (Node tag2 wallet2 total2 _ left2 right2)) =
    updateHeight $ Node tag2 wallet2 total2 h2 (updateHeight $ Node tag1 wallet1 total1 h1 left1 left2) right2
    where h1 = height left1
          h2 = height right2
rotateLeft node@(Node _ _ _ _ Empty _) = node

rotateRight :: BinaryTree -> BinaryTree
rotateRight Empty = Empty
rotateRight (Node tag1 wallet1 total1 _ (Node tag2 wallet2 total2 _ left2 right2) right1) =
    updateHeight $ Node tag2 wallet2 total2 h2 left2 (updateHeight $ Node tag1 wallet1 total1 h1 right2 right1)
    where h1 = height right1
          h2 = height left2
rotateRight node@(Node _ _ _ _ Empty _) = node


balance :: BinaryTree -> BinaryTree
balance tree@(Node tag wallet total _ left right)
    | balanceFactor tree > 1 = if balanceFactor left < 0 then rotateRight $ rotateLeft tree else rotateRight tree
    | balanceFactor tree < -1 = if balanceFactor right > 0 then rotateLeft $ rotateRight tree else rotateLeft tree
    | otherwise = tree
balance tree = tree

insert :: Wallet -> BinaryTree -> IO BinaryTree
insert wallet@(Wallet id amt) Empty = do
    tag <- nextRandom
    return $ Node tag wallet amt 1 Empty Empty
insert wallet@(Wallet id amt) (Node tag' (Wallet id' amt') total height left right) = do
    if id == id' then
        return $ Node tag' (Wallet id amt) (newTotal amt) height left right
    else
        if amt <= amt' then do
            newLeft <- insert wallet left
            let newTotal = total + amt
            return $ balance $ Node tag' (Wallet id' amt') newTotal (height + 1) newLeft right
        else do
            newRight <- insert wallet right
            let newTotal = total + amt
            return $ balance $ Node tag' (Wallet id' amt') newTotal (height + 1) left newRight
  where
    newTotal newAmt = total + newAmt

modify :: String -> Double -> BinaryTree -> Maybe BinaryTree
modify _ _ Empty = Nothing
modify name delta node@(Node tag wallet@(Wallet walletName amt) total height left right)
    | name == walletName = Just $ Node tag (Wallet walletName (amt + delta)) (total + delta) height left right
    | name < walletName = fmap (\newLeft -> balance $ Node tag wallet newTotal height newLeft right) (modify name delta left)
    | otherwise = fmap (\newRight -> balance $ Node tag wallet newTotal height left newRight) (modify name delta right)
    where newTotal = total + delta


search :: String -> BinaryTree -> Maybe Wallet
search _ Empty = Nothing
search name (Node _ wallet@(Wallet walletName _) _ _ left right)
    | name == walletName = Just wallet
    | name < walletName = search name left
    | otherwise = search name right

delete :: String -> BinaryTree -> BinaryTree
delete _ Empty = Empty
delete name node@(Node tag wallet@(Wallet walletName _) total height left right)
    | name == walletName = merge left right
    | name < walletName = balance $ Node tag wallet newTotal (height - 1) (delete name left) right
    | otherwise = balance $ Node tag wallet newTotal (height - 1) left (delete name right)
    where newTotal = total - getAmount wallet
          getAmount (Wallet _ amt) = amt
          merge Empty right = right
          merge left Empty = left
          merge left right = let (minNode, minName) = findMinNode right
                             in Node tag minNode newTotal (height - 1) left (delete minName right)
          findMinNode (Node tag' wallet@(Wallet walletName _) _ _ Empty _) = (wallet, walletName)
          findMinNode (Node tag' wallet _ _ left _) = findMinNode left

findMin :: BinaryTree -> Maybe Wallet
findMin Empty = Nothing
findMin (Node _ wallet _ _ Empty _) = Just wallet
findMin (Node _ _ _ _ left _) = findMin left

findMax :: BinaryTree -> Maybe Wallet
findMax Empty = Nothing
findMax (Node _ wallet _ _ _ Empty) = Just wallet
findMax (Node _ _ _ _ _ right) = findMax right

printTree :: BinaryTree -> Int -> String
printTree Empty _ = ""
printTree (Node uuid (Wallet name amt) total height left right) level =
  printTree right (level + 1) ++
  replicate level ' ' ++ "|--" ++
  name ++ " (" ++ show amt ++ "): " ++ "UUID=" ++ show uuid ++ ", total=" ++ show total ++ ", height=" ++ show height ++ "\n" ++
  printTree left (level + 1)

