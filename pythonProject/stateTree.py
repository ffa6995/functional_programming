
from stateMonad import State

unit = State.unit
put = State.put
get = State.get


class Node:
    def __init__(self, val):
        self.l = None
        self.r = None
        self.v = val

class Tree:
    def __init__(self):
        self.root = None

    def getRoot(self):
        return self.root

    def add(self, val):
        if self.root is None:
            self.root = Node(val)
        else:
            self._add(val, self.root)

    def _add(self, val, node):
        if val < node.v:
            if node.l is not None:
                self._add(val, node.l)
            else:
                node.l = Node(val)
        else:
            if node.r is not None:
                self._add(val, node.r)
            else:
                node.r = Node(val)

    def find(self, val):
        if self.root is not None:
            return self._find(val, self.root)
        else:
            return None

    def _find(self, val, node):
        if val == node.v:
            return node
        elif (val < node.v and node.l is not None):
            return self._find(val, node.l)
        elif (val > node.v and node.r is not None):
            return self._find(val, node.r)

    def isLeaf(self, node):
        if node.l is None and node.r is None:
            return True
        else:
            return False


    def deleteTree(self):
        # garbage collector will do this for us.
        self.root = None

    def printTree(self):
        if self.root is not None:
            self._printTree(self.root)

    def _printTree(self, node):
        if node is not None:
            self._printTree(node.l)
            print(str(node.v) + ' ')
            self._printTree(node.r)

tree = Tree()
tree.add("c")
tree.add("b")
tree.add("e")
tree.add("d")
tree.add("f")
tree.add("a")
tree.add("g")
#tree.printTree()
#print(tree.find(3).v)
#print(tree.find(10))
#tree.deleteTree()
#tree.printTree()

print("root: ", tree.root.v)

def labelTreeMonad() -> State:
    state = get()
    put((state, 1))
    state = get().bind(lambda value:
                       put(1).bind(
                          lambda _: unit(value.getRoot().v))
                       if (value.isLeaf(value))
                       else labelTreeMonad().run(value))


    return state

result = labelTreeMonad().run(tree)
print(result)