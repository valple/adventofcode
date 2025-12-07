def read_input(filename):
    with open(filename) as f:
        return f.read().splitlines()

def part_1_and_2(lines):
    nsplits = 0
    cur_beams = {lines[0].find('S'): 1}
    new_beams = {}
    allseq = []
    y = 1
    while y < len(lines):
        for j in cur_beams.keys():
            if lines[y][j] == '.':
                if j in new_beams.keys():
                    new_beams[j] += cur_beams[j]
                else:
                    new_beams[j] = cur_beams[j]
            else:
                nsplits += 1
                if j+1 in new_beams.keys():
                    new_beams[j+1] += cur_beams[j]
                else:
                    new_beams[j+1] = cur_beams[j]
                if j-1 in new_beams.keys():
                    new_beams[j-1] += cur_beams[j]
                else:
                    new_beams[j-1] = cur_beams[j]
 
        cur_beams = new_beams
        new_beams = {} 
        y += 1
    timelines = sum(cur_beams.values())
    return nsplits, timelines
       
def process_part_1(filename):
    lines = read_input(filename)
    splits, _ = part_1_and_2(lines)
    return splits

def process_part_2(filename):
    lines = read_input(filename)
    _, timelines = part_1_and_2(lines)
    return timelines

def process_both(filename):
    lines = read_input(filename)
    return part_1_and_2(lines)

def test_part_1():
    return process_part_1('testinput_7.txt')

def test_part_2():
    return process_part_2('testinput_7.txt')

def sol_part_1():
    return process_part_1('input_D07.txt')

def sol_part_2():
    return process_part_2('input_D07.txt')

def sol_both():
    return process_both('input_D07.txt')
