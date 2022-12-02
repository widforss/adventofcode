import sys

print("Answer part 1: " + str(
    sum([
        (me + 1) + ((me - opponent + 4) % 3 * 3)
        for opponent, me in [
            (ord(game[0]) - ord('A'), ord(game[2]) - ord('X'))
            for game in open(sys.argv[1]).readlines()
        ]
    ])
))
print("Answer part 2: " + str(
    sum([
        ((opponent + result - 1) % 3 + 1) + (result * 3)
        for opponent, result in [
            (ord(game[0]) - ord('A'), ord(game[2]) - ord('X'))
            for game in open(sys.argv[1]).readlines()
        ]
    ])
))
