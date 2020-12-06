import sys
from functools import reduce

print("Sum of part 1: " + str(reduce(
    lambda sum, group: sum + len(set(group) - {"\n"}),
    open(sys.argv[1]).read().rstrip().split("\n\n"),
    0
)))
print("Sum of part 2: " + str(reduce(
    lambda sum, group: sum + len(reduce(
        lambda answer_1, answer_2: set(answer_1) & set(answer_2),
        group.split()
    )),
    open(sys.argv[1]).read().rstrip().split("\n\n"),
    0
)))