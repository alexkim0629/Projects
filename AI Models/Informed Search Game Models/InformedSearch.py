############################################################
# Imports
############################################################
import random
import heapq
import math
import itertools
############################################################
# Tile Puzzle
############################################################

def create_tile_puzzle(rows, cols):
    board = []
    for i in range(rows):
        row = []
        for j in range(cols):
            value = cols * i + j + 1
            row.append(value)
        board.append(row)
    board[-1][-1] = 0
    return TilePuzzle(board)


class TilePuzzle(object):

    # Required
    def __init__(self, board):  # initialization method
        self.board = [row[:] for row in board]
        self.rows = len(board)
        self.cols = len(board[0]) if self.rows > 0 else 0
        for i in range(self.rows):
            for j in range(self.cols):
                if self.board[i][j] == 0:
                    self.empty_pos = (i, j)
                    return

    def get_board(self):
        return [row[:] for row in self.board]

    def perform_move(self, direction):
        moves = {"up": (-1, 0), "down": (1, 0), "left": (0, -1),
                 "right": (0, 1)}
        if direction not in moves:
            return False
        i, j = self.empty_pos
        di, dj = moves[direction]
        ni, nj = i + di, j + dj
        if 0 <= ni < self.rows and 0 <= nj < self.cols:
            self.board[i][j], self.board[ni][nj] = (
                self.board[ni][nj], self.board[i][j]
            )
            self.empty_pos = (ni, nj)
            return True
        return False

    def scramble(self, num_moves):
        directions = ["up", "down", "left", "right"]
        for _ in range(num_moves):
            self.perform_move(random.choice(directions))

    def is_solved(self):
        expected = []
        for i in range(self.rows):
            row = []
            for j in range(self.cols):
                value = self.cols * i + j + 1
                row.append(value)
            expected.append(row)
        expected[-1][-1] = 0
        return self.board == expected

    def copy(self):
        new_board = [row[:] for row in self.board]
        return TilePuzzle(new_board)

    def successors(self):
        directions = ["up", "down", "left", "right"]
        for direction in directions:
            new_puzzle = self.copy()
            if new_puzzle.perform_move(direction):
                yield (direction, new_puzzle)


    # Required
    def find_solutions_iddfs(self):
        def iddfs_helper(puzzle, limit, moves, visited):
            if puzzle.is_solved():
                yield moves
                return
            if limit == 0:
                return
            board_tuple = tuple(tuple(row) for row in puzzle.board)
            visited.add(board_tuple)
            for direction, new_puzzle in puzzle.successors():
                new_board_tuple = tuple(tuple(row) for row in new_puzzle.board)
                if new_board_tuple not in visited:
                    yield from iddfs_helper(
                        new_puzzle,
                        limit - 1,
                        moves + [direction],
                        visited
                    )
            visited.remove(board_tuple) # (new_board_tuple)
        max_depth = 0
        while True:
            found = False
            visited = set()
            for solution in iddfs_helper(self, max_depth, [], visited):
                found = True
                yield solution
            if found:
                break
            max_depth += 1

    # Required
    def find_solution_a_star(self):

        def manhattan(board):
            dist = 0
            for i in range(self.rows):
                for j in range(self.cols):
                    val = board[i][j]
                    if val == 0:
                        continue
                    goal_i = (val - 1) // self.cols
                    goal_j = (val - 1) % self.cols
                    dist += abs(i - goal_i) + abs(j - goal_j)
            return dist

        def find_empty(board):
            for i in range(self.rows):
                for j in range(self.cols):
                    if board[i][j] == 0:
                        return (i, j)
            return None

        # Construct the solved board for comparison
        solved_board = [
            [self.cols * i + j + 1 for j in range(self.cols)]
            for i in range(self.rows)
        ]
        solved_board[-1][-1] = 0
        solved_tuple = tuple(tuple(row) for row in solved_board)

        start_board = tuple(tuple(row) for row in self.board)
        start_empty = find_empty(self.board)
        heap = []
        counter = itertools.count()
        heapq.heappush(
            heap,
            (
                manhattan(self.board),
                next(counter),
                0,
                start_board,
                [],
                start_empty
            )
        )
        visited = set([start_board])
        while heap:
            est, _, cost, board, path, empty = heapq.heappop(heap)
            if board == solved_tuple:
                return path

            i, j = empty
            for direction, (di, dj) in {"up": (-1,0), "down": (1,0),
                            "left": (0,-1), "right": (0,1)}.items():
                ni, nj = i + di, j + dj
                if 0 <= ni < self.rows and 0 <= nj < self.cols:
                    new_board = [list(row) for row in board]
                    temp = new_board[i][j]
                    new_board[i][j] = new_board[ni][nj]
                    new_board[ni][nj] = temp
                    new_board_tuple = tuple(tuple(row) for row in new_board)
                    if new_board_tuple not in visited:
                        visited.add(new_board_tuple)
                        heapq.heappush(
                            heap,
                            (
                                cost + 1 + manhattan(new_board),
                                next(counter),
                                cost + 1,
                                new_board_tuple,
                                path + [direction],
                                (ni, nj)
                            )
                        )
        return []

############################################################
# Grid Navigation
############################################################


def find_path(start, goal, scene):
    rows, cols = len(scene), len(scene[0])
    if scene[start[0]][start[1]] or scene[goal[0]][goal[1]]:
        return None

    directions = [(-1, 0), (1, 0), (0, -1), (0, 1),
                  (-1, -1), (-1, 1), (1, -1), (1, 1)]

    def heuristic(a, b):
        return math.hypot(a[0] - b[0], a[1] - b[1])

    frontier = []
    heapq.heappush(frontier, (heuristic(start, goal), 0, start, [start]))
    visited = {}

    while frontier:
        est, cost, current, path = heapq.heappop(frontier)
        if current == goal:
            return path
        if current in visited and visited[current] <= cost:
            continue
        visited[current] = cost
        for di, dj in directions:
            ni, nj = current[0] + di, current[1] + dj
            if 0 <= ni < rows and 0 <= nj < cols and not scene[ni][nj]:
                next_pos = (ni, nj)
                step_cost = math.hypot(di, dj)
                new_cost = cost + step_cost
                if (
                    next_pos not in visited or
                    new_cost < visited.get(next_pos, float('inf'))
                ):
                    heapq.heappush(
                        frontier,
                        (
                            new_cost + heuristic(next_pos, goal),
                            new_cost,
                            next_pos,
                            path + [next_pos]
                        )
                    )
    return None

############################################################
# Linear Disk Movement, Revisited
############################################################


def solve_distinct_disks(length, n):
    import heapq
    import itertools

    if n < 0 or length <= 0 or n > length:
        return None

    start = tuple(i+1 if i < n else 0 for i in range(length))
    goal = tuple([0] * (length - n) + list(reversed(range(1, n+1))))

    if start == goal:
        return []

    def perform_move(state, frm, to):
        new = list(state)
        disk = new[frm]
        new[frm] = 0
        new[to] = disk
        return tuple(new)

    def successors(state):
        for pos, disk in enumerate(state):
            if disk == 0:
                continue
            # Move left
            if pos-1 >= 0 and state[pos-1] == 0:
                yield (pos, pos-1), perform_move(state, pos, pos-1)
            # Move right
            if pos+1 < length and state[pos+1] == 0:
                yield (pos, pos+1), perform_move(state, pos, pos+1)
            # Jump left
            if pos-2 >= 0 and state[pos-1] != 0 and state[pos-2] == 0:
                yield (pos, pos-2), perform_move(state, pos, pos-2)
            # Jump right
            if pos+2 < length and state[pos+1] != 0 and state[pos+2] == 0:
                yield (pos, pos+2), perform_move(state, pos, pos+2)

    def heuristic(state):
        h = 0
        for idx, disk in enumerate(state):
            if disk != 0:
                goal_pos = length - disk
                dist = abs(idx - goal_pos)
                h += dist // 2 + dist % 2
        return h

    heap = []
    counter = itertools.count()
    heapq.heappush(heap, (heuristic(start), next(counter), 0, start, []))
    visited = set()
    visited.add(start)

    while heap:
        est_total, _, cost, state, path = heapq.heappop(heap)
        if state == goal:
            return path
        for move, new_state in successors(state):
            if new_state not in visited:
                visited.add(new_state)
                new_path = path + [move]
                g = cost + 1
                h = heuristic(new_state)
                heapq.heappush(
                    heap,
                    (g + h, next(counter), g, new_state, new_path)
                )
    return []
