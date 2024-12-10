use super::helpers::read_lines;
use itertools::Itertools; 

fn parse_line(line: &str) -> Vec<i32> {
    line.split_whitespace()
	.map(|x| x.parse::<i32>().unwrap())
	.collect()
}

fn diff_vector(num_line: &[i32]) -> Vec<i32> {
    num_line.into_iter()
	.tuple_windows()
	.map(|(x, y)| x - y)
	.collect::<Vec<_>>()
}

fn is_safe_vector(v: &[i32]) -> bool {
    if !(v.iter().any(|x| (*x == 0) | (x.abs() > 3))) &
	(v.iter().all(|x| *x > 0) | v.iter().all(|x| *x < 0)) {
	    return true;
	} else {
	    return false;
	}
}

pub fn part_1() -> usize {
    if let Ok(lines) = read_lines("./inp2.txt") {
	lines.flatten()
	    .map(|x| parse_line(&x))
	    .map(|x| diff_vector(&x))
	    .filter(|x| is_safe_vector(&x))
	    .count()
    } else {
	0	
    }
}

fn is_safe_dampenend(v: &[i32]) -> bool {
    let vlen = v.len();
    if is_safe_vector(v) |
    is_safe_vector(&v[1..vlen]) |
    is_safe_vector(&v[..(vlen - 1)]) {
	return true;
    }
    for x in 0..(vlen - 1) {
	let mut stupidvec = v.to_vec();
	stupidvec[x] = v[x] + v[x + 1];
	stupidvec.remove(x+1);
	if is_safe_vector(&stupidvec) {
	    return true;
	}
    }
    return false;
}

pub fn part_2() -> usize {
    if let Ok(lines) = read_lines("./inp2.txt") {
	lines.flatten()
	    .map(|x| parse_line(&x))
	    .map(|x| diff_vector(&x))
	    .filter(|x| is_safe_dampenend(&x))
	    .count()
    } else {
	0	
    }
}
