import sys


def cd(args, _out, cwd, dirs):
    if args == ["/"]:
        return [], dirs
    if args == [".."]:
        return cwd[:-1], dirs
    return cwd + [args[0]], dirs


def ls(_args, out, cwd, dirs):
    for type, name in out:
        if type != "dir":
            for i in range(len(cwd) + 1):
                full_name = f"/{'/'.join(cwd[:i])}"
                dirs[full_name] = dirs.get(full_name, 0) + int(type)
    return cwd, dirs


def exec_cmd(cmds, cwd, dirs):
    if not cmds:
        return dirs
    cmd, args, out = cmds[0][0][0], cmds[0][0][1:], cmds[0][1:]
    if cmd in ["cd", "ls"]:
        cwd, dirs = globals()[cmd](args, out, cwd, dirs)
    return exec_cmd(cmds[1:], cwd, dirs)


input = [
    [line.split() for line in cmd.split("\n")[:-1]]
    for cmd in open(sys.argv[1]).read().split("$ ")[1:]
]

dirs = exec_cmd(input, [], {})
print("Answer part 1: " + str(
    sum([size for size in dirs.values() if size <= 100000])
))

to_free = dirs["/"] + 30000000 - 70000000
print("Answer part 2: " + str(
    min([size for size in dirs.values() if size >= to_free])
))
