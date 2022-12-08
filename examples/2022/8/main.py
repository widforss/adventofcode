import sys

tree_map = [
    [(x, y, int(z)) for x, z in enumerate(line)]
    for (y, line) in enumerate(open(sys.argv[1]).read().split("\n"))
]

visible_trees = set()
scenic_score = {}
for _ in range(4):
    for line in tree_map:
        candidates = []
        for (i, (x, y, z)) in enumerate(line):
            candidates = [(x_, y_, z_) for (x_, y_, z_) in candidates if z < z_]
            candidates.append((x, y, z))

            distance = 0
            for (x_, y_, z_) in line[i + 1:]:
                distance += 1
                if z_ >= z:
                    break

            scenic_score[(x, y)] = scenic_score.get((x, y), 1) * distance
        visible_trees = visible_trees | set((x, y) for (x, y, z) in candidates)
    tree_map = list(zip(*reversed(tree_map)))

print("Answer part 1: " + str(len(visible_trees)))
print("Answer part 2: " + str(max(scenic_score.values())))
