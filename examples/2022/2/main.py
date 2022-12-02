import sys

print("Answer part 1: " + str(
    sum([
        (res[1] + 1) + ((res[1] - res[0] + 4) % 3 * 3)
        for res in [
            (ord(game[0]) - ord('A'), ord(game[2]) - (ord('X')))
            for game in open(sys.argv[1]).read().split("\n")
        ]
    ])
))
print("Answer part 2: " + str(
    sum([
        ((res[0] + res[1] - 1) % 3 + 1) + (res[1] * 3)
        for res in [
            (ord(game[0]) - ord('A'), ord(game[2]) - (ord('X')))
            for game in open(sys.argv[1]).read().split("\n")
        ]
    ])
))
