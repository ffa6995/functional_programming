# This is a sample Python script.

# Press Shift+F10 to execute it or replace it with your code.
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.


def test():
    # Immutable Data
    x = 100
    print('Type: ', type(x), '\n',
          'Memory id: ', id(x))
    #Type:  <class 'int'>
    #Memory id:  140715477443440

    x = 200
    print('Type: ', type(x), '\n',
          'Memory id: ', id(x))
    # Type:  <class 'int'>
    # Memory id:  140715477446640

    x = float(x)
    print('Type: ', type(x), '\n',
          'Memory id: ', id(x))
    # Type:  <class 'float'>
    # Memory id:  1974826931504

    myList = [1, 2, 3, 4, 5]
    print('Type: ', type(list), '\n',
          'Memory id: ', id(list))
    # Type:  <class 'type'>
    # Memory id:  140715477207088

    myList = [12, 33, 11, 42]
    print('Type: ', type(list), '\n',
          'Memory id: ', id(list))
    # Type:  <class 'type'>
    # Memory id:  140715477207088

## Higher Order Functions


    # return a function



# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    test()


    # pass function to function
    def calculateSum(nums):
        sum = 0
        for num in nums:
            sum += num
        return sum


    def calculateProduct(nums):
        prod = 1
        for num in nums:
            prod = prod * num
        return prod


    def calculator(func, nums):
        solution = func(nums)
        print(solution)
        return solution

    nums = [1, 2, 3, 4]
    calcSum = calculateSum
    calcProduct = calculateProduct
    calculator(calcSum, nums)
    calculator(calcProduct, nums)

    # type variables
    value = 12
    print(type(value)) #<class 'int'>
    value = "Haskell"
    print(type(value)) #<class 'str'>
    value = ["H", "as", "kell"] #<class 'list'>
    print(type(value))


    # lambda expression

    def myfunc(n):
        return lambda a: a * n

    mydoubler = myfunc(2)
    mytripler = myfunc(3)

    print(mydoubler(11)) #22
    print(mytripler(11)) #33

    # Program to filter out only the even items from a list
    my_list = [1, 5, 4, 6, 8, 11, 3, 12]

    new_list = list(filter(lambda x: (x % 2 == 0), my_list))

    print(new_list) #[4, 6, 8, 12]
    # 100
    # Currying
    def add(a):
        def add_a(b):
            return a + b
        return add_a

    # Currying with lambda
    add_4 = lambda a: lambda b: lambda c: lambda d: a + b + c + d

    print(add(4)(5)) #9
    print(add_4(2)(4)(3)(1)) #10

    ## ADT
    from dataclasses import dataclass
    from typing import Union, NoReturn

    @dataclass(frozen=True)
    class OK:
        result: int

    @dataclass(frozen=True)
    class Failure:
        msg: str

    Result = Union[OK, Failure]

    def assert_never(x: NoReturn) -> NoReturn:
        raise AssertionError("Unhandled type: {}".format(type(x).__name__))

    def showResult(r: Result) -> str:
        if isinstance(r, OK):
            return str(r.result)
        elif isinstance(r, Failure):
            return "Failure: " + r.msg
        else:
            assert_never(r)

    ok = OK(123)
    fail = Failure("Failure")
    print(showResult(ok)) #123
    print(showResult(fail)) #Failure: Failure


# See PyCharm help at https://www.jetbrains.com/help/pycharm/
