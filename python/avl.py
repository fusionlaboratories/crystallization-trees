class Wallet:
    def __init__(self, wallet_id, balance):
        self.wallet_id = wallet_id
        self.balance = balance

    def __str__(self):
        return f"Wallet ID: {self.wallet_id}, Balance: {self.balance}"

class AVLNode:
    def __init__(self, wallet):
        self.wallet = wallet
        self.left = None
        self.right = None
        self.height = 1

class AVLTree:
    def __init__(self):
        self.root = None
        self.total_balance = 0

    def insert(self, wallet):
        self.root = self._insert(self.root, wallet)
        self.total_balance += wallet.balance

    def _insert(self, node, wallet):
        if not node:
            return AVLNode(wallet)

        if wallet.wallet_id < node.wallet.wallet_id:
            node.left = self._insert(node.left, wallet)
        else:
            node.right = self._insert(node.right, wallet)

        node.height = 1 + max(self._get_height(node.left), self._get_height(node.right))

        return self._balanced_node(node)

    def delete(self, wallet_id):
        wallet, self.root = self._delete(self.root, wallet_id)
        if wallet:
            self.total_balance -= wallet.balance
        return wallet

    def _delete(self, node, wallet_id):
        if not node:
            return None, None

        deleted_wallet = None
        if wallet_id < node.wallet.wallet_id:
            deleted_wallet, node.left = self._delete(node.left, wallet_id)
        elif wallet_id > node.wallet.wallet_id:
            deleted_wallet, node.right = self._delete(node.right, wallet_id)
        else:
            if not node.left:
                return node.wallet, node.right
            elif not node.right:
                return node.wallet, node.left
            else:
                temp_wallet = self._find_min(node.right)
                node.wallet = temp_wallet
                _, node.right = self._delete(node.right, temp_wallet.wallet_id)

        node.height = 1 + max(self._get_height(node.left), self._get_height(node.right))

        return deleted_wallet, self._balanced_node(node)

    def search(self, wallet_id):
        return self._search(self.root, wallet_id)

    def _search(self, node, wallet_id):
        if not node:
            return None

        if wallet_id < node.wallet.wallet_id:
            return self._search(node.left, wallet_id)
        elif wallet_id > node.wallet.wallet_id:
            return self._search(node.right, wallet_id)
        else:
            return node.wallet

    def modify(self, wallet_id, new_balance):
        wallet = self.search(wallet_id)
        if wallet:
            self.total_balance += new_balance - wallet.balance
            wallet.balance = new_balance

    def _get_height(self, node):
        if not node:
            return 0

        return node.height

    def _get_balance(self, node):
        if not node:
            return 0

        return self._get_height(node.left) - self._get_height(node.right)

    def _balanced_node(self, node):
        if self._get_balance(node) > 1:
            if self._get_balance(node.left) < 0:
                node.left = self._rotate_left(node.left)
            node = self._rotate_right(node)
        elif self._get_balance(node) < -1:
            if self._get_balance(node.right) > 0:
                node.right = self._rotate_right(node.right)
            node = self._rotate
