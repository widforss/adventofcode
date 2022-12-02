import sys
from functools import reduce

print("Answer part 1: " + str(
    reduce(
        lambda ack, result: ack + (result[0] + 1) + ((result[1] + 4) % 3) * 3,
        [
            (ord(game[2]) - ord('X'), ord(game[2]) - ord(game[0]) - (ord('X') - ord('A')))
            for game in open(sys.argv[1]).read().split("\n")
        ],
        0
    )
))
print("Answer part 2: " + str(
    reduce(
        lambda ack, result: ack + ((result[0] + result[1] - 1) % 3 + 1) + (result[1] * 3),
        [
            (ord(game[0]) - ord('A'), ord(game[2]) - (ord('X')))
            for game in open(sys.argv[1]).read().split("\n")
        ],
        0
    )
))
