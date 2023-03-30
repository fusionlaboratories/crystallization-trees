class TreeNode:
    def __init__(self, key, value):
        self.key = key
        self.value = value
        self.left = None
        self.right = None

def combine_balance(balance1, balance2):
    return balance1 + balance2

def create_summary(root):
    if root is None:
        return 0
    left_sum = create_summary(root.left)
    right_sum = create_summary(root.right)
    return combine_balance(combine_balance(left_sum, right_sum), root.value)

def query_summary(root, start_key, end_key):
    if root is None:
        return 0
    if root.key < start_key:
        return query_summary(root.right, start_key, end_key)
    elif root.key > end_key:
        return query_summary(root.left, start_key, end_key)
    else:
        left_sum = query_summary(root.left, start_key, end_key)
        right_sum = query_summary(root.right, start_key, end_key)
        return combine_balance(combine_balance(left_sum, right_sum), root.value)
