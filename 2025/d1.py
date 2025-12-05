from itertools import accumulate

def read_input(filename):
    f = open(filename, "r")
    lines  = f.read().splitlines()
    return lines

# Or just replace L by - and R by space and then call int
def line_to_value(line):
    return int(line.replace('R', '', 1).replace('L', '-', 1))
    
def line_apply(vals, line_val):
    newvals = divmod(vals[1] + line_val, 100)
    if newvals[0] < 0 and vals[1] == 0 and newvals[1] != 0:
        return (newvals[0] + 1, newvals[1])
    elif newvals[1] == 0 and vals[1] != 0 and newvals[0] < 0:
        return (newvals[0] - 1, newvals[1])
    else:
        return newvals
 
def parse_lines(filename):
    lines = read_input(filename)
    startval = (0, 50)
    numlines  = [line_to_value(x) for x in lines]
    seqvals = accumulate(numlines, line_apply, initial = startval)
    return seqvals

def process_part_1(filename):
    seqvals = parse_lines(filename)
    return len(list(filter(lambda vals: vals[1] == 0, seqvals)))

def process_part_2(filename):
    seqvals = parse_lines(filename)
    return sum([max(abs(y[0]), 1) if y[1] == 0 else abs(y[0]) for y in list(seqvals)])

def sol_part_1_testinput():
    return process_part_1('testinput_1.txt')
 
def sol_part_1():
    return process_part_1('input_D01.txt')

def sol_part_2_testinput():
    return process_part_2('testinput_1.txt')
 
def sol_part_2():
    return process_part_2('input_D01.txt')

def sol_both():
    return sol_part_1(), sol_part_2()
