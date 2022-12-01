import sys

print("Answer part 1: " + str(
    max([
        sum([
            int(calorie)
            for calorie in calories.split()
        ])
        for calories in open(sys.argv[1]).read().split("\n\n")
    ])
))
print("Answer part 2: " + str(
    sum(sorted([
        sum([
            int(calorie)
            for calorie in calories.split()
        ])
        for calories in open(sys.argv[1]).read().split("\n\n")
    ])[-3:])
))
