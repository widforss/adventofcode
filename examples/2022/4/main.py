import sys

print("Answer part 1: " + str(
    sum([
        min1 <= min2 and max1 >= max2 or min2 <= min1 and max2 >= max1
        for (min1, max1), (min2, max2) in [
            [map(int, assignment.split('-')) for assignment in pair.split(',')]
            for pair in open(sys.argv[1]).read().split('\n')
        ]
    ])
))
print("Answer part 2: " + str(
    sum([
        not (max1 < min2 or max2 < min1)
        for (min1, max1), (min2, max2) in [
            [map(int, assignment.split('-')) for assignment in pair.split(',')]
            for pair in open(sys.argv[1]).read().split('\n')
        ]
    ])
))
