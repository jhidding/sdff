# ~/~ begin <<docs/ch2-dsl.md#python/sdff/combinators.py>>[init]
from collections.abc import Callable
from functools import reduce

def compose_2[T, U, V](f: Callable[[U], V], g: Callable[[T], U]) -> Callable[[T], V]:
    """Composes two function of a single argumeent."""
    def composed(a: T) -> V:
        return f(g(a))

    return composed

def compose(*args):
    return reduce(compose_2, args)

# ~/~ begin <<docs/ch2-dsl.md#combinators-python>>[init]
def splat[*Ts, U](f: Callable[[*Ts], U]) -> Callable[[tuple[*Ts]],U]:
    return lambda a: f(*a)
# ~/~ end
# ~/~ begin <<docs/ch2-dsl.md#combinators-python>>[1]
def vmap(f):
    def vmapped(*args):
        return map(f, args)
    return vmapped
# ~/~ end
# ~/~ end
