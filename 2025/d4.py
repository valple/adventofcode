import itertools

def read_input(filename):
    with open(filename) as f:
       lines = f.read().splitlines()
       return [list(x) for x in lines]

def nbhd_iter():
    return filter(lambda x: not(x[0] == 0 and x[1] == 0),
                  itertools.product(range(-1, 2), range(-1, 2)))

def nbhd_grid(lines):
    xmax =  len(lines)
    ymax = len(lines[0])
    ngrid = [[0 for i in range(xmax)] for j in range(ymax)]
    for i, j in itertools.product(range(xmax), range(ymax)):
        if lines[i][j] == '@':
            for a, b in nbhd_iter():
                if 0 <= i + a < xmax and 0 <= j + b < ymax:
                    ngrid[i+a][j+b] += 1
    return ngrid

def part_1(lines):
    ngrid = nbhd_grid(lines)
    sum = 0
    for i, j in itertools.product(range(len(lines)), range(len(lines[0]))):
        if lines[i][j] == '@' and ngrid[i][j] < 4:
            sum += 1
    return sum

def nbhd_grid(lines):
    xmax =  len(lines)
    ymax = len(lines[0])
    ngrid = [[0 for i in range(xmax)] for j in range(ymax)]
    for i, j in itertools.product(range(xmax), range(ymax)):
        if lines[i][j] == '@':
            for a, b in nbhd_iter():
                if 0 <= i + a < xmax and 0 <= j + b < ymax:
                    ngrid[i+a][j+b] += 1
    return ngrid


def part_2(lines):
    ngrid = nbhd_grid(lines)
    sum = 0
    xmax =  len(lines)
    ymax = len(lines[0])
    oldsum = -1
    while sum != oldsum:
        oldsum = sum
        for i, j in itertools.product(range(xmax), range(ymax)):
            if lines[i][j] == '@' and ngrid[i][j] < 4:
                lines[i][j] = 'x'
                sum += 1
                for a, b in nbhd_iter():
                    if 0 <= i + a < xmax and 0 <= j + b < ymax:
                        ngrid[i+a][j+b] -= 1
    return sum
   
def process_part_1(filename):
    lines = read_input(filename)
    res = part_1(lines)
    return res

def process_part_2(filename):
    lines = read_input(filename)
    res = part_2(lines)
    return res 
               
def test_part_1():
    return process_part_1('testinput_4.txt')

def sol_part_1():
    return process_part_1('input_D04.txt')

def test_part_2():
    return process_part_2('testinput_4.txt')

def sol_part_2():
    return process_part_2('input_D04.txt')

def sol_both():
    return sol_part_1(), sol_part_2()
