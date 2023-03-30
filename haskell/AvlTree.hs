module AvlTree
  ( Wallet(..)
  , AVLTree(..)
  , insert
  , modifyBalance
  , deleteByAddress
  , rotateLeft
  , rotateRight
  , visualizeTree
  ) where
{-# LANGUAGE DeriveFoldable #-}
import Data.List (foldl',intercalate)

data Wallet = Wallet
  { walletAddress :: String
  , walletBalance :: Double
  } deriving (Show, Eq)

data AVLTree a = Empty | Node a (AVLTree a) (AVLTree a) Double deriving (Show, Eq, Foldable)

instance Ord Wallet where
  compare a b = compare (walletBalance a) (walletBalance b)

treeBalance :: AVLTree a -> Double
treeBalance Empty = 0
treeBalance (Node _ _ _ bal) = bal

updateBalance :: AVLTree Wallet -> AVLTree Wallet
updateBalance Empty = Empty
updateBalance (Node v l r _) = Node v l r newBalance
  where newBalance = walletBalance v + treeBalance l + treeBalance r

rotateRight :: AVLTree Wallet -> AVLTree Wallet
rotateRight (Node p (Node q a b _) c _) = updateBalance (Node q a (Node p b c (-1)) (-1))
rotateRight t = t

rotateLeft :: AVLTree Wallet -> AVLTree Wallet
rotateLeft (Node q a (Node p b c _) _) = updateBalance (Node p (Node q a b (-1)) c (-1))
rotateLeft t = t

balance :: AVLTree Wallet -> AVLTree Wallet
balance node@(Node v l r _)
  | treeBalance l - treeBalance r > walletBalance (findMax l) = 
      if treeBalance r > treeBalance l
        then rotateRight (Node v (rotateLeft l) r (treeBalance node)) 
        else rotateRight node
  | treeBalance r - treeBalance l > walletBalance (findMax r) = 
      if treeBalance l > treeBalance r
        then rotateLeft (Node v l (rotateRight r) (treeBalance node)) 
        else rotateLeft node
  | otherwise = node
balance t = t


insert :: Wallet -> AVLTree Wallet -> AVLTree Wallet
insert v Empty = Node v Empty Empty (walletBalance v)
insert v node@(Node k l r h)
  | v == k = node
  | v < k = balance (Node k (insert v l) r (-1))
  | otherwise = balance (Node k l (insert v r) (-1))

modifyBalance :: String -> Double -> AVLTree Wallet -> AVLTree Wallet
modifyBalance _ _ Empty = Empty
modifyBalance addr balance (Node w@(Wallet wAddr bal) l r h)
  | addr == wAddr = Node (Wallet addr (bal + balance)) l r h
  | addr < wAddr = Node w (modifyBalance addr balance l) r h
  | otherwise = Node w l (modifyBalance addr balance r) h

delete :: Wallet -> AVLTree Wallet -> AVLTree Wallet
delete _ Empty = Empty
delete x (Node v l r h)
  | x < v = balance $ Node v (delete x l) r (-1)
  | x > v = balance $ Node v l (delete x r) (-1)
  | otherwise = case (l, r) of
      (Empty, _) -> r
      (_, Empty) -> l
      (_, _) -> let (maxL, newL) = deleteMax l in balance $ Node maxL newL r (-1)

deleteMax :: AVLTree Wallet -> (Wallet, AVLTree Wallet)
deleteMax Empty = error "Cannot delete maximum from empty tree"
deleteMax (Node v l Empty _) = (v, l)
deleteMax (Node v l r _) = let (maxR, newR) = deleteMax r in (maxR, balance $ Node v l newR (-1))


deleteMin :: AVLTree Wallet -> (Wallet, AVLTree Wallet)
deleteMin Empty = error "Cannot delete minimum from empty tree"
deleteMin (Node v Empty r _) = (v, r)
deleteMin (Node v l r _) = let (minL, newL) = deleteMin l in (minL, balance $ Node v newL r (-1))



findMax :: AVLTree Wallet -> Wallet
findMax Empty = error "Cannot find maximum in empty tree"
findMax (Node v _ Empty _) = v
findMax (Node _ _ r _) = if r == Empty then v else findMax r


deleteByAddress :: String -> AVLTree Wallet -> AVLTree Wallet
deleteByAddress _ Empty = Empty
deleteByAddress addr (Node w@(Wallet wAddr _) l r _)
  | addr == wAddr = delete w (Node w l r (-1))
  | addr < wAddr = Node w (deleteByAddress addr l) r (-1)
  | otherwise = Node w l (deleteByAddress addr r) (-1)

visualizeTree :: Show a => AVLTree a -> Int -> String
visualizeTree Empty _ = ""
visualizeTree (Node v left right _) level =
  visualizeTree right (level + 1) ++
  replicate level ' ' ++ "|--" ++ show v ++ "\n" ++
  visualizeTree left (level + 1)


