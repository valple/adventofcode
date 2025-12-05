from functools import reduce

def read_input(filename):
    f = open(filename, "r")
    lines  = f.read().splitlines()
    return lines

def parse_line_part_1(line):
    numlist = [int(x) for x in list(line)]
    ind1 = 0
    ind2 = 1
    for i in range(2, len(numlist)):
        if numlist[ind1] >= numlist[ind2]:
            if numlist[i] > numlist[ind2]:
                ind2 = i
        else:
            ind1 = ind2
            ind2 = i
    return (10 * numlist[ind1]) + numlist[ind2]

def which_remove(dirty13):
    for i in range(0, 12):
        if dirty13[i] < dirty13[i+1]:
            return i
    return 12 

def parse_line_part_2(line):
    numlist = [int(x) for x in list(line)]
    dirtydozen = []
    for i, x in enumerate(numlist):
        if i < 12:
            dirtydozen.append(x)
        else:
            dirtydozen.append(x)
            del_ind = which_remove(dirtydozen)
            del dirtydozen[del_ind]
    sum = 0
    for i, x in enumerate(dirtydozen):
       sum += (10**(11-i))*x
    return sum

def process_part_1(filename):
    lines = read_input(filename)
    res = sum(map(parse_line_part_1, lines))
    return res

def process_part_2(filename):
    lines = read_input(filename)
    res = sum(map(parse_line_part_2, lines))
    return res

def test_part_1():
    return process_part_1('testinput_3.txt')

def sol_part_1():
    return process_part_1('input_D03.txt')
def test_part_2():
    return process_part_2('testinput_3.txt')

def sol_part_2():
    return process_part_2('input_D03.txt')

def sol_both():
    return sol_part_1(), sol_part_2()
