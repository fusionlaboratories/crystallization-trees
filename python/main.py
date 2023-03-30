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

def main():
    num_data_points = 1000

    test_data = generate_test_data(num_data_points)

    tree = SelfBalancingBinaryTree()

    # Insert test data into the tree
    start = timer()
    for balance, utxo in test_data:
        tree.insert(balance, utxo)
    end = timer()
    insert_time = end - start

    # Calculate the sum of balances
    start = timer()
    balance_sum = tree.sum_balance_to_root(tree.root)
    end = timer()
    sum_time = end - start

    print(f"Insert time: {insert_time:.6f}s")
    print(f"Sum time: {sum_time:.6f}s")
    print(f"Sum of balances: {balance_sum}")

if __name__ == "__main__":
    main()
