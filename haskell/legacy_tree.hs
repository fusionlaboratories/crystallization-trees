import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

data BinaryTree = Empty
                | Node UUID Wallet Double BinaryTree BinaryTree deriving (Show)

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

-- Search for a wallet in the tree
search :: Wallet -> BinaryTree -> Maybe Double
search (Wallet id amt) Empty = Nothing
search (Wallet id amt) (Node (Wallet id' amt') total left right)
    | id == id' = Just total
    | amt <= amt'  = search (Wallet id amt) left
    | otherwise = search (Wallet id amt) right

-- Delete a wallet from the tree
delete :: Wallet -> BinaryTree -> BinaryTree
delete (Wallet id amt) Empty = Empty
delete (Wallet id amt) (Node (Wallet id' amt') total left right)
    | amt < amt'  = Node (Wallet id' amt') newTotal (delete (Wallet id amt) left) right
    | amt > amt'  = Node (Wallet id' amt') newTotal left (delete (Wallet id amt) right)
    | id /= id'   = Node (Wallet id' amt') total (delete (Wallet id amt) left) right
    | otherwise = case (left, right) of
                    (Empty, Empty) -> Empty
                    (Empty, right) -> right
                    (left, Empty) -> left
                    (left, right) -> let minNode = findMinNode right
                                     in Node minNode newTotal left (delete minNode right)
    where newTotal = total - amt
          findMinNode (Node (Wallet id' amt') _ Empty _) = Wallet id' amt'
          findMinNode (Node (Wallet id' amt') _ left _) = findMinNode left

-- Find the wallet with the minimum amount in the tree
findMin :: BinaryTree -> Maybe Wallet
findMin Empty = Nothing
findMin (Node wallet _ Empty _) = Just wallet
findMin (Node _ _ left _) = findMin left

-- Find the wallet with the maximum amount in the tree
findMax :: BinaryTree -> Maybe Wallet
findMax Empty = Nothing
findMax (Node wallet _ _ Empty) = Just wallet
findMax (Node _ _ _ right) = findMax right
