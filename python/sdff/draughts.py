# ~/~ begin <<docs/ch2-dsl.md#python/sdff/draughts.py>>[init]
"""
Implements the rules for the game of Draughts (not Checkers!)
"""

from collections.abc import Iterable
from enum import StrEnum
from dataclasses import dataclass
from functools import reduce


def cartesian_product_2[T, U](a: Iterable[T], b: Iterable[U]) -> Iterable[tuple[T, U]]:
    return ((i, j) for i in a for j in b)


def cartesian_product(*args):
    return reduce(cartesian_product_2, args)


class Colour(StrEnum):
    WHITE = "white"
    BLACK = "black"


type Pos = tuple[int, int]

@dataclass
class Board:
    size: int
    turn: Colour
    fields: bytearray

    @staticmethod
    def start_position(size):
        pass

    def on_board(self, pos: Pos) -> bool:
        pass
# ~/~ end
