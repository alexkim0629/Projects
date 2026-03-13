############################################################
# Imports
############################################################
import collections
import copy
import itertools
import random
import math

############################################################
# Dominoes Game
############################################################

def create_dominoes_game(rows, cols):
    board = [[False for _ in range(cols)] for _ in range(rows)]
    return DominoesGame(board)


class DominoesGame(object):

    def __init__(self, board):
        self.board = board
        self.rows = len(board)
        self.cols = len(board[0]) if self.rows > 0 else 0

    def get_board(self):
        return self.board

    def reset(self):
        self.board = [
            [False for _ in range(self.cols)]
            for _ in range(self.rows)
        ]

    def is_legal_move(self, row, col, vertical):
        if vertical:
            if row < 0 or row >= self.rows - 1 or col < 0 or col >= self.cols:
                return False
            return not self.board[row][col] and not self.board[row+1][col]
        else:
            if row < 0 or row >= self.rows or col < 0 or col >= self.cols - 1:
                return False
            return not self.board[row][col] and not self.board[row][col+1]

    def legal_moves(self, vertical):
        for r in range(self.rows):
            for c in range(self.cols):
                if self.is_legal_move(r, c, vertical):
                    yield (r, c)

    def perform_move(self, row, col, vertical):
        if vertical:
            self.board[row][col] = True
            self.board[row+1][col] = True
        else:
            self.board[row][col] = True
            self.board[row][col+1] = True

    def game_over(self, vertical):
        return all(
            not self.is_legal_move(r, c, vertical)
            for r in range(self.rows)
            for c in range(self.cols)
        )

    def copy(self):
        new_board = copy.deepcopy(self.board)
        return DominoesGame(new_board)

    def successors(self, vertical):
        for move in self.legal_moves(vertical):
            new_game = self.copy()
            new_game.perform_move(move[0], move[1], vertical)
            yield (move, new_game)

    def get_best_move(self, vertical, limit):
        def max_value(game, vertical, alpha, beta, depth):
            if game.game_over(vertical) or depth == 0:
                util = evaluate(game, vertical)
                return util, None, 1
            v = float('-inf')
            best_move = None
            leaf_total_count = 0
            for move, new_game in game.successors(vertical):
                v2, _, leaf_count = min_value(
                    new_game, not vertical, alpha, beta, depth - 1
                )
                leaf_total_count += leaf_count
                if v2 > v:
                    v, best_move = v2, move
                alpha = max(alpha, v)
                if alpha >= beta:
                    break
            return v, best_move, leaf_total_count

        def min_value(game, vertical, alpha, beta, depth):
            if game.game_over(vertical) or depth == 0:
                util = evaluate(game, not vertical)
                return util, None, 1
            v = float('inf')
            best_move = None
            leaf_total_count = 0
            for move, new_game in game.successors(vertical):
                v2, _, leaf_count = max_value(
                    new_game, not vertical, alpha, beta, depth - 1
                )
                leaf_total_count += leaf_count
                if v2 < v:
                    v, best_move = v2, move
                beta = min(beta, v)
                if beta <= alpha:
                    break
            return v, best_move, leaf_total_count

        def evaluate(game, vertical):
            my_moves = sum(1 for _ in game.legal_moves(vertical))
            opp_moves = sum(1 for _ in game.legal_moves(not vertical))
            return my_moves - opp_moves

        value, best_move, leaf_count = max_value(
            self, vertical, float('-inf'), float('inf'), limit
        )
        if best_move is None:
            return (0, 0), value, leaf_count
        return best_move, value, leaf_count
