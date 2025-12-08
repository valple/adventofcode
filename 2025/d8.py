import itertools

def read_input(filename):
    with open(filename) as f:
        lines = f.read().splitlines()
        return [[int(x) for x in y.split(',')] for y in lines]

def dist_euc_sq(v, w):
    return sum(map(lambda i: (w[i] - v[i])**2, range(len(v))))

def sorted_distances_2(lines):
    distances = map(lambda y: [dist_euc_sq(y[0][1], y[1][1]), y[0][0], y[1][0]],
                    filter(lambda x: x[0] < x[1],
                           itertools.product(enumerate(lines), repeat = 2)))
    return sorted(distances)

def sorted_distances(lines):
    inds = filter(lambda x: x[0] < x[1], itertools.product(range(len(lines)), repeat = 2))
    return sorted(inds, key = lambda x: dist_euc_sq(lines[x[0]], lines[x[1]]))

def testsd():
    return sorted_distances(read_input('input_D08.txt'))

def testsd2():
    return sorted_distances_2(read_input('input_D08.txt'))


def part_1(lines, distances, ncons = 1000):
    circuits = []
    newcircuit = True
    for i in range(ncons):
        x, y = distances[i]
        cinds = [j for j, c in enumerate(circuits) if x in c or y in c]
        if len(cinds) == 0:
            circuits.append([x, y])
        elif len(cinds) == 1:
            circuits[cinds[0]].append(x)
            circuits[cinds[0]].append(y)
        else:
            circuits[min(cinds)] = circuits[cinds[0]] + circuits[cinds[1]]
            del circuits[max(cinds)]
    sorted_circuits = sorted([set(y) for y in circuits], key = lambda x: 1/len(x))
    return len(sorted_circuits[0]) * len(sorted_circuits[1]) * len(sorted_circuits[2])

def part_2(lines, distances):
    circuits = []
    newcircuit = True
    i = 0
    while i < len(distances):
        x, y = distances[i]
        cinds = [j for j, c in enumerate(circuits) if x in c or y in c]
        if len(cinds) == 0:
            circuits.append([x, y])
        elif len(cinds) == 1:
            circuits[cinds[0]].append(x)
            circuits[cinds[0]].append(y)
        else:
            circuits[min(cinds)] = circuits[cinds[0]] + circuits[cinds[1]]
            del circuits[max(cinds)]
        if len(set(circuits[0])) == len(lines):
            return lines[x][0] * lines[y][0]
        i += 1
    return 0
    
def process_part_1(filename, ncons = 1000):
    lines = read_input(filename)
    distances = sorted_distances(lines)
    return part_1(lines, distances, ncons)

def process_part_2(filename):
    lines = read_input(filename)
    distances = sorted_distances(lines)
    return part_2(lines, distances) 

def process_both(filename, ncons = 1000):
    lines = read_input(filename)
    distances = sorted_distances(lines)
    return part_1(lines, distances, ncons), part_2(lines, distances)
    
def test_part_1():
    return process_part_1('testinput_8.txt', 10)

def test_part_2():
    return process_part_2('testinput_8.txt')

def sol_part_1():
    return process_part_1('input_D08.txt')

def sol_part_2():
    return process_part_2('input_D08.txt')

def sol_both():
    return process_both('input_D08.txt')
