import sys


def update_pc_signal_crt(pc, x, signal, crt):
    if (pc + 20) % 40 == 0:
        signal = signal + [pc * x]
    crt = crt + ('#' if abs((pc - 1) % 40 - x) <= 1 else ' ')
    return pc + 1, signal, crt


def noop(pc, x, signal, crt, _args):
    pc, signal, crt = update_pc_signal_crt(pc, x, signal, crt)
    return pc, x, signal, crt


def addx(pc, x, signal, crt, args):
    for _ in range(2):
        pc, signal, crt = update_pc_signal_crt(pc, x, signal, crt)
    return pc, x + args, signal, crt


program = [
    (line[:4], int(line[5:]) if len(line) > 5 else None)
    for line in open(sys.argv[1]).read().split('\n')
]

pc = x = 1
signal = []
crt = ''
for instr, arg in program:
    if instr in ["noop", "addx"]:
        pc, x, signal, crt = globals()[instr](pc, x, signal, crt, arg)

print("Answer part 1:", sum(signal))
print("Answer part 2:")
for i in range(len(crt) // 40):
    print(crt[40*i:40*i+40])
