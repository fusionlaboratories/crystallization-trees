module Main where

import AvlTree
import Data.List (foldl')

sampleWallets :: [Wallet]
sampleWallets =
  [ Wallet "address1" 100
  , Wallet "address2" 200
  , Wallet "address3" 150
  , Wallet "address4" 50
  , Wallet "address5" 250
  ]

main :: IO ()
main = do
  let tree = foldl' (flip insert) Empty sampleWallets
  putStrLn "Initial AVL Tree:"
  putStrLn $ visualizeTree tree 0
  
  let modifiedTree = modifyBalance "address3" (-50) tree
  putStrLn "\nModified AVL Tree:"
  putStrLn $ visualizeTree modifiedTree 0
  
  let insertedTree = insert (Wallet "address6" 175) modifiedTree
  putStrLn "\nAVL Tree after inserting a new wallet:"
  putStrLn $ visualizeTree insertedTree 0
  
  let deletedTree = deleteByAddress "address4" insertedTree
  putStrLn "\nAVL Tree after deleting a wallet:"
  putStrLn $ visualizeTree deletedTree 0
  
  let rotatedLeftTree = rotateLeft deletedTree
  putStrLn "\nAVL Tree after rotating left:"
  putStrLn $ visualizeTree rotatedLeftTree 0
  
  let rotatedRightTree = rotateRight rotatedLeftTree
  putStrLn "\nAVL Tree after rotating right:"
  putStrLn $ visualizeTree rotatedRightTree 0
