from functools import partial, reduce
from math import sqrt
from operator import add, mul
from itertools import takewhile

print(f"2^5 = {pow(2,5)}")

# With partial it's like Haskell's currying function
pow2 = partial(pow, 2)
print(f"2^5 with partial = {pow2(5)}")

# Callable objects
class Multiplier:
    def __init__(self, to_mul) -> None:
        self._to_mul = to_mul # Initialize the private variable _to_mul
    
    def __call__(self, val) -> int:
        return self._to_mul * val

triple = Multiplier(3)
print(f"Compute 3*5 with a callable function = {triple(5)}")

# Map function: apply a function to a list
values = [0,1,2,3,4]
values2 = range(2,7)
print(f"Apply sqrt to {values} produces\n {list(map(sqrt, values))}")
print(f"Apply add to {values} and {list(values2)} produces\n {list(map(add, values, values2))}")
print(f"Apply mul to {values} and {list(values2)} produces\n {list(map(mul, values, values2))}")

# Lamba function
v = [5,7,-2,1,9,6]
r = sorted(v)
print(f"Sorted {v}: {r}")
print(f"Sorted {v} by increasing 1/x: {sorted(v, key=lambda x: 1/x)}")

# Variadic functions
def func(required_arg, *args, **kwargs):
    print(required_arg)
    # args is a tuple of positional arguments
    if args:
        print(args)
    # kwargs is a dictionary of kewords arguments
    if kwargs:
        print(kwargs)

    
func("ra", 1,2,5,'3', key1=4, key2='hello')

# Filtering in Python
es = "".join(takewhile(lambda x: x !=" ", "elephants know how to party"))  # Take the first word of a sentence
print(es)
es2 = "".join(x for x in "IdontLIKEFROGS" if x.isupper())   # a sort of list comprehension
print(es2)

# Folding
x = [3,5,2,1]
result = reduce(lambda a, b: a+b, x, 0) # applies the function a+b cumulatively on list x, with 0 as initial value
print(f"The sum of {x} is {result}")