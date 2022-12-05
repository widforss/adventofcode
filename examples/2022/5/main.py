import sys

stacks, steps = open(sys.argv[1]).read().split("\n\n")
stacks = stacks.split("\n")[:-1]
stacks = [
    [level[i] for level in stacks if level[i] != ' ']
    for i in range(1, len(stacks[0]), 4)
]
steps = [
    (int(step[1]), int(step[3]) - 1, int(step[5]) - 1)
    for step in map(str.split, steps.split("\n"))
]

def exec_steps(steps, stacks, reverse):
    if not steps:
        return stacks
    mv, from_, to = steps[0]
    to_stack = stacks[from_][:mv][::-1 if reverse else 1] + stacks[to]
    from_stack = stacks[from_][mv:]
    stacks = [
        {to: to_stack, from_: from_stack}.get(i, stack)
        for i, stack in enumerate(stacks)
    ]
    return exec_steps(steps[1:], stacks, reverse)

print("Answer part 1: " + ''.join(
    [stack[0] for stack in exec_steps(steps, stacks, True)]
))
print("Answer part 2: " + ''.join(
    [stack[0] for stack in exec_steps(steps, stacks, False)]
))
