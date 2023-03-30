from sbt import *
from faker import Faker
import random
from time import time as timer

def generate_test_data(num_data_points):
    fake = Faker()
    test_data = []

    for _ in range(num_data_points):
        balance = random.uniform(0.01, 1000)
        utxo = fake.sha256()
        test_data.append((balance, utxo))

    return test_data

def insert_classic(test_data):
    balances = []
    for balance, _ in test_data:
        balances.append(balance)
    return balances

def sum_classic(balances):
    total = 0
    for balance in balances:
        total += balance
    return total

def main():
    num_data_points = 1000

    test_data = generate_test_data(num_data_points)

    # Classic insertion
    start = timer()
    balances = insert_classic(test_data)
    end = timer()
    classic_insert_time = end - start

    # Classic sum
    start = timer()
    classic_sum = sum_classic(balances)
    end = timer()
    classic_sum_time = end - start

    tree = SelfBalancingBinaryTree()

    # AVL insertion
    start = timer()
    for balance, utxo in test_data:
        tree.insert(balance, utxo)
    end = timer()
    avl_insert_time = end - start

    # AVL sum
    start = timer()
    avl_sum = tree.sum_balance_to_root(tree.root)
    end = timer()
    avl_sum_time = end - start

    print("Classic Insertion:")
    print(f"Insert time: {classic_insert_time:.6f}s")
    print()

    print("AVL Insertion:")
    print(f"Insert time: {avl_insert_time:.6f}s")
    print()

    print("Classic Sum:")
    print(f"Sum time: {classic_sum_time:.6f}s")
    print(f"Sum of balances: {classic_sum}")
    print()

    print("AVL Sum:")
    print(f"Sum time: {avl_sum_time:.6f}s")
    print(f"Sum of balances: {avl_sum}")

if __name__ == "__main__":
    main()
