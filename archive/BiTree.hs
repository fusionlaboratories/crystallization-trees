module BiTree (Wallet(..), BinaryTree(..), insert, findMin, findMax) where
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Control.Applicative ((<|>))

data BinaryTree = Empty
                | Node UUID Wallet Double BinaryTree BinaryTree deriving (Show)
-- ... (rest of the code)

instance Eq BinaryTree where
    Empty == Empty = True
    (Node _ _ _ _ _) == Empty = False
    Empty == (Node _ _ _ _ _) = False
    (Node tag1 wallet1 _ left1 right1) == (Node tag2 wallet2 _ left2 right2) =
        tag1 == tag2 && wallet1 == wallet2 && left1 == left2 && right1 == right2

instance Ord BinaryTree where
    (Node _ _ amt1 _ _) `compare` (Node _ _ amt2 _ _) = amt1 `compare` amt2

data Wallet = Wallet String Double deriving (Eq, Show)

-- Insert a new wallet into the tree with a unique tag
insert :: Wallet -> BinaryTree -> IO BinaryTree
insert (Wallet id amt) Empty = do
    tag <- nextRandom
    return $ Node tag (Wallet id amt) amt Empty Empty
insert (Wallet id amt) (Node tag' (Wallet id' amt') total left right) = do
    if id == id'
        then return $ Node tag' (Wallet id amt) total left right
        else if amt <= amt'
            then do
                newLeft <- insert (Wallet id amt) left
                return $ Node tag' (Wallet id' amt') newTotal newLeft right
            else do
                newRight <- insert (Wallet id amt) right
                return $ Node tag' (Wallet id' amt') newTotal left newRight
    where newTotal = total + amt

-- Search for a wallet in the tree by its unique tag
search :: UUID -> BinaryTree -> Maybe Wallet
search _ Empty = Nothing
search tag (Node tag' wallet total left right)
    | tag == tag' = Just wallet
    | otherwise = search tag left <|> search tag right

-- Delete a wallet from the tree by its unique tag
delete :: UUID -> BinaryTree -> BinaryTree
delete _ Empty = Empty
delete tag (Node tag' wallet total left right)
    | tag == tag' = merge left right
    | otherwise = Node tag' wallet newTotal (delete tag left) (delete tag right)
    where newTotal = total - getAmount wallet
          getAmount (Wallet _ amt) = amt
          merge Empty right = right
          merge left Empty = left
          merge left right = let (minNode, minTag) = findMinNode right
                             in Node minTag minNode newTotal left (delete minTag right)
          findMinNode (Node tag' wallet _ Empty _) = (wallet, tag')
          findMinNode (Node tag' wallet _ left _) = findMinNode left

-- Find the wallet with the minimum amount in the tree
findMin :: BinaryTree -> Maybe Wallet
findMin Empty = Nothing
findMin (Node _ wallet _ Empty _) = Just wallet
findMin (Node _ _ _ left _) = findMin left

-- Find the wallet with the maximum amount in the tree
findMax :: BinaryTree -> Maybe Wallet
findMax Empty = Nothing
findMax (Node _ wallet _ _ Empty) = Just wallet
findMax (Node _ _ _ _ right) = findMax right

