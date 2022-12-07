import sys
from copy import deepcopy


def cd(args, _out, cwd, fs, dirs):
    if args == ["/"]:
        return [], fs, dirs
    if args == [".."]:
        return cwd[:-1], fs, dirs
    return cwd + [args[0]], fs, dirs


def ls(_args, out, cwd, fs, dirs):
    node = fs = deepcopy(fs)
    dirs = deepcopy(dirs)
    for dir in cwd:
        node = node[dir]
    for type, name in out:
        if type == "dir":
            node[name] = {}
        else:
            node[name] = int(type)
            for i in range(len(cwd) + 1):
                full_name = f"/{'/'.join(cwd[:i])}"
                dirs[full_name] = dirs.get(full_name, 0) + node[name]
    return cwd, fs, dirs


def exec_cmd(cmds, cwd, fs, dirs):
    if not cmds:
        return dirs
    cmd, args, out = cmds[0][0][0], cmds[0][0][1:], cmds[0][1:]
    if cmd in ["cd", "ls"]:
        cwd, fs, dirs = globals()[cmd](args, out, cwd, fs, dirs)
    return exec_cmd(cmds[1:], cwd, fs, dirs)


input = [
    list(map(str.split, cmd.split("\n")[:-1]))
    for cmd in open(sys.argv[1]).read().split("$ ")[1:]
]

dirs = exec_cmd(input, [], {}, {})
print("Answer part 1: " + str(
    sum([size for size in dirs.values() if size <= 100000])
))

to_free = dirs["/"] + 30000000 - 70000000
print("Answer part 2: " + str(
    min([size for size in dirs.values() if size >= to_free])
))
