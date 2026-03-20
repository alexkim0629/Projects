############################################################
# Imports
############################################################

import collections
import copy
import itertools
import random
import math

############################################################
# Sudoku Solver
############################################################


def sudoku_cells():  # List[Tuple[int, int]],  return a list of cells
    return [(r, c) for r in range(9) for c in range(9)]


# return set of pairs of cells that are either in the same row, column, or subgrid
def sudoku_arcs():
    arcs = set()
    for r in range(9):
        for c in range(9):
            for d in range(9):
                if d != c:
                    arcs.add(((r, c), (r, d)))  # same row
                if d != r:
                    arcs.add(((r, c), (d, c)))  # same column
            # same subgrid
            subgrid_r = (r // 3) * 3
            subgrid_c = (c // 3) * 3
            for dr in range(3):
                for dc in range(3):
                    if (subgrid_r + dr, subgrid_c + dc) != (r, c):
                        arcs.add(((r, c), (subgrid_r + dr, subgrid_c + dc)))
    return arcs


def read_board(path):  # Dict[Tuple[int, int], Set[int]]
    board = dict()
    with open(path, 'r') as f:
        for r, line in enumerate(f):
            for c, value in enumerate(line.strip()):
                if value == '*':
                    board[(r, c)] = set(range(1, 10))
                else:
                    board[(r, c)] = {int(value)}
    return board


class Sudoku(object):

    CELLS = sudoku_cells()
    ARCS = sudoku_arcs()

    def __init__(self, board):
        self.board = board

    def get_values(self, cell):
        return self.board[cell]

    def is_solved(self):
        return all(len(self.get_values(cell)) == 1 for cell in self.CELLS)

    # -> bool
    # checks if a value in cell is inconsistent with an assignment in cell2 & removes it from the values of cell
    # return True if an inconsistent value has been removed from cell
    def remove_inconsistent_values(self, cell1, cell2):
        removed = False
        values1 = self.get_values(cell1)
        values2 = self.get_values(cell2)
        for value in set(values1):
            if len(values2) == 1 and value in values2:
                self.board[cell1].remove(value)
                removed = True
        return removed

    def infer_ac3(self):
        """
        Runs the AC-3 algorithm on the current board to narrow down each cell’s
        set of values as much as possible.
        """
        queue = collections.deque(self.ARCS)
        while queue:
            (cell1, cell2) = queue.popleft()
            if self.remove_inconsistent_values(cell1, cell2):
                for (neighbor1, neighbor2) in self.ARCS:
                    if neighbor2 == cell1 and neighbor1 != cell2:
                        queue.append((neighbor1, neighbor2))

    def infer_improved(self):
        made_add_inference = True  # initialize a variable to see if extra
        while made_add_inference:
            self.infer_ac3()
            made_add_inference = False
            for cell in self.CELLS:  # loop through each cell
                values = self.get_values(cell)
                if len(values) > 1:  # the cell has not been assigned value yet
                    row, col = cell
                    # Check row
                    for digit in self.get_values(cell):
                        if all(
                            digit not in self.get_values((row, c)) or
                            (row, c) == cell
                            for c in range(9)
                        ):
                            self.board[cell] = {digit}
                            made_add_inference = True
                            break
                    # Check column
                    for digit in self.get_values(cell):
                        if all(
                            (
                                digit not in self.get_values((r, col))
                                or (r, col) == cell
                            )
                            for r in range(9)
                        ):
                            self.board[cell] = {digit}
                            made_add_inference = True
                            break
                    # Check box
                    box_r = (row // 3) * 3
                    box_c = (col // 3) * 3
                    for digit in self.get_values(cell):
                        if all(
                            (
                                digit not in self.get_values(
                                    (box_r + dr, box_c + dc))
                                or (box_r + dr, box_c + dc) == cell
                            )
                            for dr in range(3)
                            for dc in range(3)
                        ):
                            self.board[cell] = {digit}
                            made_add_inference = True
                            break
        return True

    def infer_with_guessing(self):
        if self.is_solved():
            return True

        self.infer_improved()

        for cell in self.CELLS:
            if len(self.get_values(cell)) == 0:
                return False

        for cell in self.CELLS:
            values = self.get_values(cell)
            if len(values) > 1:
                for digit in values:
                    # make a copy of the current board
                    new_board = copy.deepcopy(self.board)
                    # set the current board cell to this value
                    new_board[cell] = {digit}
                    # recurse and solve this board
                    sudoku = Sudoku(new_board)
                    if sudoku.infer_with_guessing():
                        self.board = sudoku.board
                        return True
                return False
        return self.is_solved()
