import sys


def sign(x):
    return (x > 0) - (x < 0)


def exec_moves(moves, n_knots):
    knots = [(0, 0)] * n_knots
    visited_positions = {(0, 0)}
    for move in moves:
        ((dx, dy), steps) = move
        for _ in range(steps):
            (hx, hy) = knots[0]
            knots[0] = hx + dx, hy + dy
            for i in range(len(knots) - 1):
                [(hx, hy), (tx, ty)] = knots[i:i+2]
                if abs(hx - tx) > 1 or abs(hy - ty) > 1:
                    knots[i+1] = tx + sign(hx - tx), ty + sign(hy - ty)
            visited_positions.add(knots[-1])
    return visited_positions


moves = [
    (
        {"R": (1, 0), "U": (0, 1), "L": (-1, 0), "D": (0, -1)}[line[0]],
        int(line[2:])
    )
    for line in open(sys.argv[1]).read().split("\n")
]
print("Answer part 1:", len(exec_moves(moves, 2)))
print("Answer part 2:", len(exec_moves(moves, 10)))
