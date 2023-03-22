import BiTree (Wallet(..), BinaryTree(..), insert, findMin, findMax)

main :: IO ()
main = do
    let wallets = [ Wallet ("User" ++ show i) (fromIntegral (i * 100)) | i <- [1..10] ]

    tree <- foldr (\wallet t -> insert wallet =<< t) (pure Empty) wallets

    print tree
    print $ findMin tree
    print $ findMax tree
