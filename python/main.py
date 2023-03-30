from sbt import *

from time import time as timer

import random

def main():
    num_data_points = 10000000

    test_data = generate_test_data(num_data_points)

    # Classic insertion, deletion, modification, and sum
    start = timer()
    balances = insert_classic(test_data)
    end = timer()
    classic_insert_time = end - start

    start = timer()
    classic_sum = sum_classic(balances)
    end = timer()
    classic_sum_time = end - start

    sum_tree = AVLSumTree()

    # AVL insertion
    start = timer()
    for balance, utxo in test_data:
        sum_tree.insert(balance, utxo)
    end = timer()
    avl_insert_time = end - start

    # AVL sum before
    start = timer()
    avl_sum_before = sum_tree.get_total_sum()
    end = timer()
    avl_sum_time_before = end - start

    # Choose a random index for deletion and modification
    rand_index = random.randint(0, num_data_points - 1)

    # AVL deletion
    start = timer()
    sum_tree.delete(test_data[rand_index][0], test_data[rand_index][1])
    end = timer()
    avl_delete_time = end - start

    # AVL modification
    start = timer()
    sum_tree.modify(test_data[rand_index][0], test_data[rand_index][0] * 2, test_data[rand_index][1])
    end = timer()
    avl_modify_time = end - start

    # AVL root sum
    start = timer()
    avl_sum = sum_tree.get_total_sum()
    end = timer()
    avl_sum_time_after = end - start

    # Classic deletion
    start = timer()
    delete_classic(balances, test_data[rand_index][0])
    end = timer()
    classic_delete_time = end - start

    # Classic modification
    start = timer()
    modify_classic(balances, test_data[rand_index][0], test_data[rand_index][0] * 2)
    end = timer()
    classic_modify_time = end - start

    print("Classic Insertion and Sum:")
    print(f"Sum of balances before: {classic_sum:.6f}s")
    print(f"Insert time: {classic_insert_time:.6f}s")
    print(f"Sum time: {classic_sum_time:.6f}s")
    print(f"Sum of balances: {classic_sum}")
    print(f"Delete time: {classic_delete_time:.6f}s")
    print(f"Modify time: {classic_modify_time:.6f}s")
    print()

    print("AVL Insertion, Deletion, Modification, and Root Sum:")
    print(f"Insert time: {avl_insert_time:.6f}s")
    print(f"Sum of balances before: {avl_sum_before:.6f}s")
    print(f"Sum time before: {avl_sum_time_before:.6f}s")
    print(f"Delete time: {avl_delete_time:.6f}s")
    print(f"Modify time: {avl_modify_time:.6f}s")
    print(f"Sum time: {avl_sum_time_after:.6f}s")
    print(f"Sum of balances after: {avl_sum}")

if __name__ == "__main__":
    main()
