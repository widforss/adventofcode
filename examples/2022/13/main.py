import sys
from functools import reduce


class Packet(list):
    def __init__(self, contains):
        try:
            super().__init__(contains)
        except TypeError:
            super().__init__([contains])

    def __lt__(self, other):
        if self and other:
            if isinstance(self[0], list) or isinstance(other[0], list):
                if Packet(self[0]) != Packet(other[0]):
                    return Packet(self[0]) < Packet(other[0])
                return Packet(self[1:]) < Packet(other[1:])
            elif self[0] == other[0]:
                return Packet(self[1:]) < Packet(other[1:])
            return self[0] < other[0]
        elif not self and other:
            return True
        elif self and not other:
            return False
        return True

    @staticmethod
    def parse():
        def parser(string):
            lst, string = Parser.recursive_list(
                Parser.tag("["),
                Parser.int(),
                Parser.tag(","),
                Parser.tag("]"),
            )(string)
            return Packet(lst), string
        return parser


class Parser:
    class ParseError(ValueError):
        pass

    @classmethod
    def tag(cls, tag):
        def parser(string):
            if string[:len(tag)] == tag:
                return tag, string[len(tag):]
            raise cls.ParseError()
        return parser

    @classmethod
    def int(cls):
        def parser(string):
            def is_int(string):
                return string and '0' <= string[0] <= '9'
            if not is_int(string):
                raise cls.ParseError()
            int_str = ""
            while is_int(string):
                int_str, string = int_str + string[0], string[1:]
            return int(int_str), string
        return parser

    @staticmethod
    def parens(start, inner, end):
        def parser(string):
            _, string = start(string)
            parsed, string = inner(string)
            _, string = end(string)
            return parsed, string
        return parser

    @classmethod
    def either(cls, *inners):
        def parser(string):
            for inner_parser in inners:
                try:
                    return inner_parser(string)
                except cls.ParseError:
                    continue
            raise cls.ParseError()
        return parser

    @classmethod
    def delimited(cls, delimiter, inner):
        def parser(string):
            elems = []
            try:
                first, string = inner(string)
                elems = [first]
                while True:
                    _, string_ = delimiter(string)
                    elem, string = inner(string_)
                    elems.append(elem)
            except cls.ParseError:
                return elems, string
        return parser

    @classmethod
    def recursive_list(cls, start, inner, delimiter, end):
        def parser(string):
            return cls.parens(
                start,
                cls.delimited(
                    delimiter,
                    cls.either(
                        inner,
                        cls.recursive_list(start, inner, delimiter, end)
                    )
                ),
                end,
            )(string)
        return parser


def parse_part1():
    def parser(string):
        return Parser.delimited(
            Parser.tag("\n\n"),
            Parser.delimited(
                Parser.tag("\n"),
                Packet.parse()
            )
        )(string)
    return parser


def parse_part2():
    def parser(string):
        return Parser.delimited(
            Parser.either(Parser.tag("\n\n"), Parser.tag("\n")),
            Packet.parse()
        )(string)
    return parser


input = open(sys.argv[1]).read()

print("Answer part 1:", sum([
    i for (i, (a, b)) in enumerate(parse_part1()(input)[0], 1)
    if a < b
]))

divider_packets = [Packet([[2]]), Packet([[6]])]
sorted_packets = sorted([*parse_part2()(input)[0], *divider_packets])
print("Answer part 2:", reduce(
    lambda p, packet: p * (sorted_packets.index(packet) + 1),
    divider_packets,
    1
))
