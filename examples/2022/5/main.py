import sys

input = open(sys.argv[1]).read()
(stacks, steps) = input.split("\n\n")
stacks = stacks.split("\n")[:-1]
stacks = [
    [level[i] for level in reversed(stacks) if level[i] != ' ']
    for i in range(1, len(stacks[0]), 4)
]
steps = [
    dict(zip(step[::2], map(int, step[1::2])))
    for step in map(str.split, steps.split("\n"))
]

def exec_steps(steps, stacks, reverse):
    if not steps:
        return stacks
    step, steps = steps[0], steps[1:]
    mv, from1, to1 = step["move"], stacks[step["from"]-1], stacks[step["to"]-1]
    to2 = to1 + from1[-mv:][::-1 if reverse else 1]
    stacks = [
        {step["to"]: to2, step["from"]: from1[:-mv]}.get(i, stack)
        for i, stack in enumerate(stacks, 1)
    ]
    return exec_steps(steps, stacks, reverse)

print("Answer part 1: " + ''.join(
    [stack[-1] for stack in exec_steps(steps, stacks, True)]
))
print("Answer part 2: " + ''.join(
    [stack[-1] for stack in exec_steps(steps, stacks, False)]
))
