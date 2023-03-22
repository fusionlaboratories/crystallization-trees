module BiTree (BinaryTree(..), Wallet(..), insert, delete, findMin, findMax, printTree)
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

rotateRight :: BinaryTree -> BinaryTree
rotateRight Empty = Empty
rotateRight (Node tag1 wallet1 total1 _ (Node tag2 wallet2 total2 _ left2 right2) right1) =
    updateHeight $ Node tag2 wallet2 total2 h2 left2 (updateHeight $ Node tag1 wallet1 total1 h1 right2 right1)
    where h1 = height right1
          h2 = height left2

balance :: BinaryTree -> BinaryTree
balance tree@(Node tag wallet total _ left right)
    | balanceFactor tree > 1 = if balanceFactor left < 0 then rotateRight $ rotateLeft tree else rotateRight tree
    | balanceFactor tree < -1 = if balanceFactor right > 0 then rotateLeft $ rotateRight tree else rotateLeft tree
    | otherwise = tree
balance tree = tree

insert :: Wallet -> BinaryTree -> IO BinaryTree
insert (Wallet id amt) Empty = do
    tag <- nextRandom
    return $ Node tag (Wallet id amt) amt 1 Empty Empty
insert (Wallet id amt) (Node tag' (Wallet id' amt') total height left right) = do
    if id == id'
        then return $ Node tag' (Wallet id amt) total height left right
        else if amt <= amt'
            then do
                newLeft <- insert (Wallet id amt) left
                return $ balance $ Node tag' (Wallet id' amt') newTotal (height + 1) newLeft right
            else do
                newRight <- insert (Wallet id amt) right
                return $ balance $ Node tag' (Wallet id' amt') newTotal (height + 1) left newRight
    where newTotal = total + amt

search :: UUID -> BinaryTree -> Maybe Wallet
search _ Empty = Nothing
search tag (Node tag' wallet total _ left right)
    | tag == tag' = Just wallet
    | otherwise = search tag left <|> search tag right

delete :: UUID -> BinaryTree -> BinaryTree
delete _ Empty = Empty
delete tag (Node tag' wallet total height left right)
    | tag == tag' = merge left right
    | otherwise = balance $ Node tag' wallet newTotal (height - 1) (delete tag left) (delete tag right)
    where newTotal = total - getAmount wallet
          getAmount (Wallet _ amt) = amt
          merge Empty right = right
          merge left Empty = left
          merge left right = let (minNode, minTag) = findMinNode right
                             in Node minTag minNode newTotal (height - 1) left (delete minTag right)
          findMinNode (Node tag' wallet _ _ Empty _) = (wallet, tag')
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
printTree (Node uuid (Wallet name amount) total height left right) level =
  replicate (4 * level) ' ' ++
  name ++ " (" ++ show amount ++ "): " ++ show uuid ++ "\n" ++
  printTree left (level + 1) ++
  printTree right (level + 1)