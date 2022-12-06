import sys

def find_marker(signal, length):
    idx = length
    while len(set(signal[idx - length:idx])) < length:
        idx += 1
    return idx

input = open(sys.argv[1]).read()
print("Answer part 1: " + str(find_marker(input, 4)))
print("Answer part 2: " + str(find_marker(input, 14)))
