from faker import Faker
import random

class Node:
    def __init__(self, balance, utxo):
        self.balance = balance
        self.utxo = utxo
        self.left = None
        self.right = None
        self.height = 1

class SelfBalancingBinaryTree:
    def __init__(self):
        self.root = None

    def height(self, node):
        if node is None:
            return 0
        return node.height

    def balance_factor(self, node):
        if node is None:
            return 0
        return self.height(node.left) - self.height(node.right)

    def update_height(self, node):
        node.height = 1 + max(self.height(node.left), self.height(node.right))

    def rotate_right(self, node):
        left_child = node.left
        right_child_of_left_child = left_child.right

        left_child.right = node
        node.left = right_child_of_left_child

        self.update_height(node)
        self.update_height(left_child)

        return left_child

    def rotate_left(self, node):
        right_child = node.right
        left_child_of_right_child = right_child.left

        right_child.left = node
        node.right = left_child_of_right_child

        self.update_height(node)
        self.update_height(right_child)

        return right_child

    def insert(self, balance, utxo):
        def _insert(root, balance, utxo):
            if root is None:
                return Node(balance, utxo)

            if balance < root.balance:
                root.left = _insert(root.left, balance, utxo)
            else:
                root.right = _insert(root.right, balance, utxo)

            self.update_height(root)
            balance_factor = self.balance_factor(root)

            if balance_factor > 1 and balance < root.left.balance:
                return self.rotate_right(root)

            if balance_factor < -1 and balance > root.right.balance:
                return self.rotate_left(root)

            if balance_factor > 1 and balance > root.left.balance:
                root.left = self.rotate_left(root.left)
                return self.rotate_right(root)

            if balance_factor < -1 and balance < root.right.balance:
                root.right = self.rotate_right(root.right)
                return self.rotate_left(root)

            return root

        self.root = _insert(self.root, balance, utxo)

    def sum_balance_to_root(self, node):
        if node is None:
            return 0
        return node.balance + self.sum_balance_to_root(node.left) + self.sum_balance_to_root(node.right)
    
class AVLSumTree(SelfBalancingBinaryTree):
    def __init__(self):
        super().__init__()
        self.total_sum = 0

    def insert(self, balance, utxo):
        super().insert(balance, utxo)
        self.total_sum += balance

    def delete(self, balance, utxo):
        node = self._search(self.root, balance, utxo)
        if node:
            self.total_sum -= balance
            self.root = self._delete(self.root, balance, utxo)

    def modify(self, old_balance, new_balance, utxo):
        self.delete(old_balance, utxo)
        self.insert(new_balance, utxo)

    def get_total_sum(self):
        return self.total_sum

    def _search(self, node, balance, utxo):
        if node is None:
            return None

        if balance == node.balance and utxo == node.utxo:
            return node

        if balance < node.balance:
            return self._search(node.left, balance, utxo)

        return self._search(node.right, balance, utxo)

    def _delete(self, node, balance, utxo):
        if node is None:
            return node

        if balance < node.balance:
            node.left = self._delete(node.left, balance, utxo)
        elif balance > node.balance:
            node.right = self._delete(node.right, balance, utxo)
        else:
            if utxo == node.utxo:
                if node.left is None:
                    return node.right
                elif node.right is None:
                    return node.left

                temp = self._find_min(node.right)
                node.balance = temp.balance
                node.utxo = temp.utxo
                node.right = self._delete(node.right, temp.balance, temp.utxo)
            else:
                if node.left and utxo < node.left.utxo:
                    node.left = self._delete(node.left, balance, utxo)
                elif node.right and utxo > node.right.utxo:
                    node.right = self._delete(node.right, balance, utxo)

        self.update_height(node)
        balance_factor = self.balance_factor(node)

        if balance_factor > 1 and self.balance_factor(node.left) >= 0:
            return self.rotate_right(node)

        if balance_factor < -1 and self.balance_factor(node.right) <= 0:
            return self.rotate_left(node)

        if balance_factor > 1 and self.balance_factor(node.left) < 0:
            node.left = self.rotate_left(node.left)
            return self.rotate_right(node)

        if balance_factor < -1 and self.balance_factor(node.right) > 0:
            node.right = self.rotate_right(node.right)
            return self.rotate_left(node)

        return node

    def _find_min(self, node):
        if node.left is None:
            return node
        return self._find_min(node.left)

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

def delete_classic(balances, balance):
    for i, b in enumerate(balances):
        if b == balance:
            del balances[i]
            break

def modify_classic(balances, old_balance, new_balance):
    for i, b in enumerate(balances):
        if b == old_balance:
            balances[i] = new_balance
            break

