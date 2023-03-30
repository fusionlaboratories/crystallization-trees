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
