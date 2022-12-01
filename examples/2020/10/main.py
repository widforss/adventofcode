import sys
from functools import reduce

lines = open(sys.argv[1]).read().rstrip().split()
jolts = sorted(map(lambda x: int(x), lines))
ones = threes = 1
for i in range(0, len(jolts)-1):
    diff = jolts[i + 1] - jolts[i]
    ones += int(diff == 1)
    threes += int(diff == 3)
print(f"Part 1: {ones * threes}")

max_jolt = max(jolts)
jolts = set(jolts)
combinations = [
    1,
    int(1 in jolts),
    int(2 in jolts) + int(1 in jolts),
]
minus = lambda x: combinations[jolt - x] if jolt in jolts else 0
for jolt in range(3, max_jolt):
    combinations.append(minus(1) + minus(2) + minus(3))
print(f"Part 2: {combinations[-1]}")