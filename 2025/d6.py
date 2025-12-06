from functools import reduce

def read_input(filename):
    with open(filename) as f:
        lines = f.read().splitlines()
        ops = lines[len(lines) - 1]
        nums = lines[:len(lines) - 1]
        return ops, nums

def parse_ops_2(ops):
    curop = '+'
    cur_start_pos = 0
    cur_col_len = 0
    sep_ops = []
    for i, x in enumerate(ops):
        if x == ' ':
            cur_col_len += 1
        else:
            if cur_col_len > 0:
                sep_ops.append((curop, cur_start_pos, cur_start_pos + cur_col_len - 1))
            cur_start_pos = i
            curop = x
            cur_col_len = 1
    sep_ops.append((curop, cur_start_pos, cur_start_pos + cur_col_len))
    return sep_ops

def part_1(ops, nums):
    ops = ops.split()
    nums = [[int(x) for x in s.split()] for s in nums]
    sum = 0
    for j in range(0, len(ops)):
        if ops[j] == '+':
            tmpfn = lambda x, y: x + y
        else:
            tmpfn = lambda x, y: x * y
        sum += reduce(lambda a, b: tmpfn(a, b), map(lambda x: x[j], nums))
    return sum

def process_part_1(filename):
    ops, nums = read_input(filename)
    return part_1(ops, nums)

def part_2(ops, nums):
    ops = parse_ops_2(ops)
    sum = 0
    for symb, start, stop in ops:
        if symb == '+':
            tmpfn = lambda x, y: x + y
            val = 0
        else:
            tmpfn = lambda x, y: x * y
            val = 1
        for i in range(start, stop):
            val = tmpfn(val, int(reduce(lambda s, t: s + t, map(lambda x: x[i], nums))))
        sum += val
    return sum

def process_part_2(filename):
    ops, nums = read_input(filename)
    return part_2(ops, nums)

def test_part_1():
    return process_part_1('testinput_6.txt')

def test_part_2():
    return process_part_2('testinput_6.txt')

def sol_part_1():
    return process_part_1('input_D06.txt')

def sol_part_2():
    return process_part_2('input_D06.txt')

def sol_both():
    return sol_part_1(), sol_part_2()
