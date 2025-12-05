def read_input(filename):
    ranges = []
    ids = []
    foundempty = False
    with open(filename) as f:
        for line in f.read().splitlines():
            if foundempty:
                ids.append(int(line))
            else:
                if line == '':
                    foundempty = True
                else:
                    ranges.append(tuple(map(int, line.split('-'))))
    return ranges, ids

def in_tuple(n, t):
    return t[0] <= n <= t[1]

def in_any_range(n, ranges):
    for t in ranges:
        if in_tuple(n, t):
            return True
    return False

def tuple_intersect(t1, t2):
    if t1[1] < t2[0] or t2[1] < t1[0]:
        return False
    else:
        return True

def tuple_union(t1, t2):
    return min(t1[0], t2[0]), max(t1[1], t2[1])

def process_part_1(filename):
    ranges, ids = read_input(filename)
    return sum(map(lambda n: in_any_range(n, ranges), ids))


def part_2_brute_force(ranges):
    max_fresh = max(map(lambda x: x[1], ranges))
    sum = 0
    for n in range(0, max_fresh + 1):
        if in_any_range(n, ranges):
            sum += 1
    return sum

def merge_ranges(ranges):
    ind = 0
    ranges.sort()
    while ind < len(ranges) - 1:
        if tuple_intersect(ranges[ind], ranges[ind+1]):
            newtuple = tuple_union(ranges[ind], ranges[ind+1])
            ranges[ind] = newtuple
            del ranges[ind + 1]
        else:
            ind += 1
    return ranges

def process_part_2(filename):
    ranges, _ = read_input(filename)
    ranges_union = merge_ranges(ranges)
    return sum(map(lambda x: x[1] - x[0] + 1, ranges_union))

def test_part_1():
    return process_part_1('testinput_5.txt')

def test_part_2():
    return process_part_2('testinput_5.txt')

def sol_part_1():
    return process_part_1('input_D05.txt')

def sol_part_2():
    return process_part_2('input_D05.txt')

def sol_both():
    return sol_part_1(), sol_part_2()
