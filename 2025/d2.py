import os

def read_input(filename):
    with open(filename, "r") as f:
        inp = f.read().splitlines()[0]
        return inp.split(',')

def is_invalid_id_part_1(n):
    s = str(n)
    slen = len(s)
    if s[0:(slen//2)] == s[(slen//2):slen]:
        return n
    else:
        return 0

def is_invalid_id_part_2(n):
    s = str(n)
    slen = len(s)
    for i in range(1, 1 + (slen // 2)):
        a, m = divmod(slen, i)
        if m == 0:
            if s[0:i] * a == s:
                return n
    return 0

def range_str_to_iter(s):
    a, b = [int(x) for x in s.split('-')]
    return range(a, b + 1)

def sum_invalids(id_range, invalid_fn):
    sum = 0
    for id in id_range:
        sum += invalid_fn(id)
    return sum

def process_part_1(filename):
    inp = read_input(filename)
    sum = 0
    for s in inp:
        id_range = range_str_to_iter(s)
        sum += sum_invalids(id_range, is_invalid_id_part_1)
    return(sum)

def process_part_2(filename):
    inp = read_input(filename)
    sum = 0
    for s in inp:
        id_range = range_str_to_iter(s)
        sum += sum_invalids(id_range, is_invalid_id_part_2)
    return(sum)

def test_part_1():
    return process_part_1('testinput_2.txt')

def sol_part_1():
    return process_part_1('input_D02.txt')

def test_part_2():
    return process_part_2('testinput_2.txt')

def sol_part_2():
    return process_part_2('input_D02.txt')

def sol_both():
    return sol_part_1(), sol_part_2()
