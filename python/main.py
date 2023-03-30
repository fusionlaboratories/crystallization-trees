from faker import Faker
import random
import timeit
from avl import Wallet, AVLTree

def test_avl_tree_insert(wallets):
    avl_tree = AVLTree()

    # Insert wallets
    for wallet in wallets:
        avl_tree.insert(wallet)

    return avl_tree

def test_list_insert(wallets):
    wallet_list = []

    # Insert wallets
    for wallet in wallets:
        wallet_list.append(wallet)

    return wallet_list

def test_avl_tree_modify(avl_tree, wallets):
    # Modify wallets
    for wallet in wallets:
        avl_tree.modify(wallet.wallet_id, random.uniform(0, 10000))

def test_list_modify(wallet_list, wallets):
    # Modify wallets
    for wallet in wallets:
        found_wallet = next((w for w in wallet_list if w.wallet_id == wallet.wallet_id), None)
        if found_wallet:
            found_wallet.balance = random.uniform(0, 10000)

def test_avl_tree_delete(avl_tree, wallets):
    # Delete wallets
    for wallet in wallets:
        avl_tree.delete(wallet.wallet_id)

def test_list_delete(wallet_list, wallets):
    # Delete wallets
    for wallet in wallets:
        wallet_list = [w for w in wallet_list if w.wallet_id != wallet.wallet_id]

def test_avl_tree_root_sum(avl_tree):
    # Calculate root sum (total balance)
    return avl_tree.total_balance

def test_list_root_sum(wallet_list):
    # Calculate root sum (total balance)
    return sum(wallet.balance for wallet in wallet_list)

fake = Faker()
num_wallets = 92643
wallets = [Wallet(fake.uuid4(), random.uniform(0, 10000)) for _ in range(num_wallets)]

avl_tree = test_avl_tree_insert(wallets)
list_wallets = test_list_insert(wallets)

# Insertions
insert_root_sum_avl_tree_time = timeit.timeit(lambda: test_avl_tree_root_sum(avl_tree), number=1)
insert_root_sum_list_time = timeit.timeit(lambda: test_list_root_sum(list_wallets), number=1)

test_avl_tree_modify(avl_tree, wallets)
test_list_modify(list_wallets, wallets)

# Modifications
modify_root_sum_avl_tree_time = timeit.timeit(lambda: test_avl_tree_root_sum(avl_tree), number=1)
modify_root_sum_list_time = timeit.timeit(lambda: test_list_root_sum(list_wallets), number=1)

test_avl_tree_delete(avl_tree, wallets)
test_list_delete(list_wallets, wallets)

# Deletions
delete_root_sum_avl_tree_time = timeit.timeit(lambda: test_avl_tree_root_sum(avl_tree), number=1)
delete_root_sum_list_time = timeit.timeit(lambda: test_list_root_sum(list_wallets), number=1)

print(f"AVLTree root sum calculation time after insertions: {insert_root_sum_avl_tree_time} seconds")
print(f"List root sum calculation time after insertions: {insert_root_sum_list_time} seconds")
print(f"AVLTree root sum calculation time after modifications: {modify_root_sum_avl_tree_time} seconds")
print(f"List root sum calculation time after modifications: {modify_root_sum_list_time} seconds")
print(f"AVLTree root sum calculation time after deletions: {delete_root_sum_avl_tree_time} seconds")
print(f"List root sum calculation time after modifications: {modify_root_sum_list_time} seconds")
print(f"AVLTree root sum calculation time after deletions: {delete_root_sum_avl_tree_time} seconds")
print(f"List root sum calculation time after deletions: {delete_root_sum_list_time} seconds")
