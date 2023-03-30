import random
from faker import Faker
from timeit import default_timer as timer

fake = Faker()


class Wallet:
    def __init__(self, balance):
        self.balance = balance
        self.wallet_id = fake.uuid4()

    def __str__(self):
        return f"Wallet ID: {self.wallet_id}, Balance: {self.balance}"


class UTXO:
    def __init__(self, wallet_id, amount):
        self.wallet_id = wallet_id
        self.amount = amount
        self.tx_id = fake.uuid4()

    def __str__(self):
        return f"Transaction ID: {self.tx_id}, Wallet ID: {self.wallet_id}, Amount: {self.amount}"


class AVLNode:
    def __init__(self, wallet):
        self.wallet = wallet
        self.left = None
        self.right = None
        self.height = 1


class AVLTree:
    def __init__(self):
        self.root = None

    def insert(self, root, wallet):
        if not root:
            return AVLNode(wallet)

        if wallet.wallet_id < root.wallet.wallet_id:
            root.left = self.insert(root.left, wallet)
        else:
            root.right = self.insert(root.right, wallet)

        root.height = 1 + max(self.get_height(root.left), self.get_height(root.right))

        balance = self.get_balance(root)

        if balance > 1:
            if wallet.wallet_id < root.left.wallet.wallet_id:
                return self.rotate_right(root)
            else:
                root.left = self.rotate_left(root.left)
                return self.rotate_right(root)

        if balance < -1:
            if wallet.wallet_id > root.right.wallet.wallet_id:
                return self.rotate_left(root)
            else:
                root.right = self.rotate_right(root.right)
                return self.rotate_left(root)

        return root

    def get_height(self, root):
        if not root:
            return 0
        return root.height

    def get_balance(self, root):
        if not root:
            return 0
        return self.get_height(root.left) - self.get_height(root.right)

    def rotate_left(self, z):
        y = z.right
        T2 = y.left

        y.left = z
        z.right = T2

        z.height = 1 + max(self.get_height(z.left), self.get_height(z.right))
        y.height = 1 + max(self.get_height(y.left), self.get_height(y.right))

        return y

    def rotate_right(self, y):
        x = y.left
        T2 = x.right

        x.right = y
        y.left = T2

        y.height = 1 + max(self.get_height(y.left), self.get_height(y.right))
        x.height = 1 + max(self.get_height(x.left), self.get_height(x.right))

        return x

    def in_order(self, root, wallet_balances):
        if root:
            self.in_order(root.left, wallet_balances)
            wallet_balances.append(root.wallet.balance)
            self.in_order(root.right, wallet_balances)

        return wallet_balances


def generate_wallets_and_utxos(num_wallets, num_utxos):
    wallets = [Wallet(random.uniform(0, 1000)) for _ in range(num_wallets)]
    utxos = [UTXO(fake.uuid4(), random.uniform(0, 100)) for _ in range(num_utxos)]
    return wallets, utxos

def linear_sum(wallets, utxos):
    wallet_balance_sum = sum(wallet.balance for wallet in wallets)
    utxo_sum = sum(utxo.amount for utxo in utxos)
    return wallet_balance_sum, utxo_sum

def avl_sum(wallets, utxos):
    wallet_tree = AVLTree()
    for wallet in wallets:
        wallet_tree.root = wallet_tree.insert(wallet_tree.root, wallet)
        
    wallet_balances = wallet_tree.in_order(wallet_tree.root, [])
    wallet_balance_sum = sum(wallet_balances)
    
    utxo_tree = AVLTree()
    for utxo in utxos:
        utxo_wallet = Wallet(utxo.amount)
        utxo_tree.root = utxo_tree.insert(utxo_tree.root, utxo_wallet)
        
    utxo_amounts = utxo_tree.in_order(utxo_tree.root, [])
    utxo_sum = sum(utxo_amounts)
    
    return wallet_balance_sum, utxo_sum

def search_linear(wallets, wallet_id):
    for wallet in wallets:
        if wallet.wallet_id == wallet_id:
            return wallet
    return None

def search_avl(root, wallet_id):
    if not root:
        return None

    if wallet_id < root.wallet.wallet_id:
        return search_avl(root.left, wallet_id)
    elif wallet_id > root.wallet.wallet_id:
        return search_avl(root.right, wallet_id)
    else:
        return root.wallet

def delete_linear(wallets, wallet_id):
    for i, wallet in enumerate(wallets):
        if wallet.wallet_id == wallet_id:
            wallets.pop(i)
            break

def delete_avl(root, wallet_id):
    if not root:
        return root

    if wallet_id < root.wallet.wallet_id:
        root.left = delete_avl(root.left, wallet_id)
    elif wallet_id > root.wallet.wallet_id:
        root.right = delete_avl(root.right, wallet_id)
    else:
        if not root.left:
            return root.right
        elif not root.right:
            return root.left

        temp = root.right
        while temp.left:
            temp = temp.left

        root.wallet = temp.wallet
        root.right = delete_avl(root.right, temp.wallet.wallet_id)

    if not root:
        return root

    root.height = 1 + max(AVLTree().get_height(root.left), AVLTree().get_height(root.right))
    balance = AVLTree().get_balance(root)

    if balance > 1:
        if AVLTree().get_balance(root.left) < 0:
            root.left = AVLTree().rotate_left(root.left)
        return AVLTree().rotate_right(root)

    if balance < -1:
        if AVLTree().get_balance(root.right) > 0:
            root.right = AVLTree().rotate_right(root.right)
        return AVLTree().rotate_left(root)

    return root

def sum_wallet_balances_linear(wallets):
    total = 0
    for wallet in wallets:
        total += wallet.balance
    return total

def sum_utxo_values_linear(utxos):
    total = 0
    for utxo in utxos:
        total += utxo.amount
    return total

def sum_avl_tree(root):
    if not root:
        return 0
    return root.wallet.balance + sum_avl_tree(root.left) + sum_avl_tree(root.right)
