import sys
from functools import reduce

print("Answer part 1: " + str(
    reduce(
        lambda ack, res: ack + (res[1] + 1) + ((res[1] - res[0] + 4) % 3) * 3,
        [
            (ord(game[0]) - ord('A'), ord(game[2]) - (ord('X')))
            for game in open(sys.argv[1]).read().split("\n")
        ],
        0
    )
))
print("Answer part 2: " + str(
    reduce(
        lambda ack, res: ack + ((res[0] + res[1] - 1) % 3 + 1) + (res[1] * 3),
        [
            (ord(game[0]) - ord('A'), ord(game[2]) - (ord('X')))
            for game in open(sys.argv[1]).read().split("\n")
        ],
        0
    )
))
