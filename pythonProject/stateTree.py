from stateMonad import State

unit = State.unit
put = State.put
get = State.get


class Tree:
    def __init__(self, leftTree, value, rightTree):
        self.leftTree = None
        self.rightTree = None
        self.value = value

    def add(self, val):
        if self.value is None:
            self.value = val
        else:
            self._add(val, self.value)

    def _add(self, val, node):
        leftTree = self.leftTree
        rightTree = self.rightTree
        nodeValue = node
        if isinstance(node, Tree):
            nodeValue = node.value
            leftTree = node.leftTree
            rightTree = node.rightTree
        if val < nodeValue:
            if leftTree is not None:
                self._add(val, leftTree)
            else:
                if isinstance(node, Tree):
                    node.leftTree = Tree(None, val, None)
                else:
                    self.leftTree = Tree(None, val, None)
        else:
            if rightTree is not None:
                self._add(val, rightTree)
            else:
                if isinstance(node, Tree):
                    node.rightTree = Tree(None, val, None)
                else:
                    self.rightTree = Tree(None, val, None)

    def printTree(self):
        if self.value is not None:
            self._printTree(self)

    def _printTree(self, node):
        if node is not None:
            self._printTree(node.leftTree)
            print(str(node.value) + ' ')
            self._printTree(node.rightTree)

    def find(self, val):
        if self.value is not None:
            return self._find(val, self.value)
        else:
            return None

    def _find(self, val, node):
        if val == self.value:
            return node
        elif val < self.value and self.leftTree is not None:
            return self._find(val, self.leftTree)
        elif val > self.value and self.rightTree is not None:
            return self._find(val, self.rightTree)

    def getRoot(self):
        return self

    def deleteTree(self):
        # garbage collector will do this for us.
        self.value = None
        self.leftTree = None
        self.rightTree = None

    def isLeaf(self):
        if self.leftTree is None and self.rightTree is None:
            return True
        else:
            return False


tree = Tree(None, "c", None)
tree.add("b")
tree.add("e")
tree.add("d")
tree.add("f")
tree.add("a")
tree.add("g")
tree.printTree()


# print(tree.find(3).v)
# print(tree.find(10))
# tree.deleteTree()
# tree.printTree()

class LabelPair:
    def __init__(self, num, val):
        self.num = Number(num)
        self.val = val

    def getNum(self):
        return self.num


class Number:
    def __init__(self, num):
        self.num = num


def labelTreeMonad(fstNode) -> State:
    if fstNode is not None and not fstNode.isLeaf():
        state = get().bind(
            lambda value: labelTreeMonad(fstNode.leftTree).bind(lambda _: labelTreeMonad(fstNode.rightTree)).bind(
                lambda _: put(value + 1).bind(lambda _: unit(fstNode.value))))
        print(state.run(0))
        return state
    elif fstNode is not None and fstNode.isLeaf():
        # newState = get().bind(lambda value:
        #                      put(i + 1).bind(
        #                          lambda _: unit(value)))
        print("in else is leaf")
        newState = get()
        print(newState)
        return newState
    elif fstNode is None:
        newState = get()
        return newState


number = LabelPair(0, "")

result = labelTreeMonad(tree)
print("fertig:", result)
