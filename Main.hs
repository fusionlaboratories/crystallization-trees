module Main where

import BiTree
import Control.Monad (forM_, forM)
import System.Random (randomRIO)

main :: IO ()
main = do
    let walletNames = ["Alice", "Bob", "Charlie", "David", "Eve", "Frank", "Grace", "Heidi", "Ivan", "Judy"]

    wallets <- forM walletNames $ \name -> do
        balance <- randomRIO (10, 1000) :: IO Double
        return $ Wallet name balance

    tree <- foldl (>>=) (return Empty) (map insert wallets)

    putStrLn "\nInitial tree:"
    putStrLn $ printTree tree 0

    -- Modify wallet balances
    let modifiedTree = foldl (\tree (name, delta) -> maybe tree id (modify name delta tree)) tree [("Alice", 500), ("Bob", -200)]
    putStrLn "\nTree after modifying wallet balances:"
    putStrLn $ printTree modifiedTree 0

    -- Search for wallets
    putStrLn "\nSearching for wallets:"
    forM_ walletNames $ \name -> do
        putStrLn $ "Searching for " ++ name ++ ": " ++ show (search name modifiedTree)

    -- Delete wallets
    let deletedTree = foldl (\tree name -> delete name tree) modifiedTree ["Alice", "Bob"]
    putStrLn "\nTree after deleting wallets:"
    putStrLn $ printTree deletedTree 0

    -- Find the minimum and maximum wallet balances
    putStrLn "\nMinimum wallet balance:"
    putStrLn $ show $ findMin deletedTree
    putStrLn "\nMaximum wallet balance:"
    putStrLn $ show $ findMax deletedTree
