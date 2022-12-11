import sys
from functools import reduce
from math import gcd
from re import search, findall


class Monkey:
    inspected_items = 0
    monkey_lcm = None

    def __init__(self, spec, monkeys, boredom_divisor=None):
        self._monkeys = monkeys
        self._boredom_divisor = boredom_divisor
        self._items = [
            int(wl) for wl in search(
                "Starting items: (((\d+)(, )?)+)", spec
            )[1].split(", ")
        ]
        self._dest_map = {
            True if path[0] == "true" else False: int(path[1])
            for path in findall("If (true|false): throw to monkey (\d+)", spec)
        }

        self.test_divisor = int(search("Test: divisible by (\d+)", spec)[1])
        self._test = lambda worry: worry % self.test_divisor == 0

        op_match = search("Operation: new = old ([*+]) (\d+|old)", spec)
        self._operator = int.__add__ if op_match[1] == '+' else int.__mul__
        self._operand = None if op_match[2] == 'old' else int(op_match[2])

    def catch(self, item):
        self._items.append(item)

    def run_turn(self):
        self.inspected_items += len(self._items)
        self._items = [
            self._operator(
                worry, worry if self._operand is None else self._operand
            ) % self.monkey_lcm
            for worry in self._items
        ]
        if self._boredom_divisor is not None:
            self._items = [
                worry // self._boredom_divisor for worry in self._items
            ]
        for dest in [self._dest_map[self._test(worry)] for worry in self._items]:
            self._monkeys[dest].catch(self._items.pop(0))


def lcm(numbers):
    return reduce(lambda ack, n: ack * n, numbers) // reduce(gcd, numbers)


def solve(input, rounds, boredom_divisor=None):
    monkeys = []
    for monkey_input in input:
        monkeys.append(Monkey(monkey_input, monkeys, boredom_divisor))
    monkey_lcm = lcm([monkey.test_divisor for monkey in monkeys])
    for monkey in monkeys:
        monkey.monkey_lcm = monkey_lcm
    for _ in range(rounds):
        for monkey in monkeys:
            monkey.run_turn()
    monkeys.sort(key=lambda monkey: monkey.inspected_items, reverse=True)
    return monkeys[0].inspected_items * monkeys[1].inspected_items


input = open(sys.argv[1]).read().split("\n\n")
print("Answer part 1:", solve(input, 20, 3))
print("Answer part 2:", solve(input, 10000))
