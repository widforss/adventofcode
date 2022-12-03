import sys

print("Answer part 1: " + str(
    sum([
        [*range(ord('a'), ord('z') + 1), *range(ord('A'), ord('Z') + 1)].index(
            ord(pocket)
        ) + 1
        for pack in open(sys.argv[1]).read().split('\n')
        for pocket in set(pack[:len(pack)//2]) & set(pack[len(pack)//2:])
    ])
))

indata = iter(open(sys.argv[1]).read().split('\n'))
print("Answer part 2: " + str(
    sum([
        [*range(ord('a'), ord('z') + 1), *range(ord('A'), ord('Z') + 1)].index(
            ord(badge)
        ) + 1
        for pack in indata
        for badge in set(pack) & set(next(indata)) & set(next(indata))
    ])
))
