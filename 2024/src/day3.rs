use regex::Regex;
use std::fs;

fn extract_res_all(s: &str) -> i32 {
    let mut res = 0;
    let re = Regex::new(r"mul\(([0-9][0-9]{0,2}),([0-9][0-9]{0,2})\)").unwrap(); //overkill? \d3
    for caps in re.captures_iter(s) {
	let (_, [x, y]) = caps.extract();
	res = res + (x.parse::<i32>().unwrap() * y.parse::<i32>().unwrap());
    }
    return res;
}

fn extract_allowed(s: &str) -> i32 {
    let mut res = 0;
    let mut status = true;
    let re = Regex::new(r"do\(\)|don't\(\)|mul\(([0-9][0-9]{0,2}),([0-9][0-9]{0,2})\)").unwrap();
    for caps in re.captures_iter(s) {
	if &caps[0] == "do()" {
	    status = true;
	} else if &caps[0] == "don't()" {
	    status = false;
	} else {
	    if status {
		res = res + (caps[1].parse::<i32>().unwrap() * caps[2].parse::<i32>().unwrap());
	    }
	}
    }
    return res;
}

pub fn part_1() -> i32 {
    if let Ok(inp) = fs::read_to_string("inp3.txt") {
	let res = extract_res_all(&inp);
	println!("Day 3 part 1: {}", res);
	res
    } else {
	-999 // really necessary but could learn something at some point
    }
}

pub fn part_2() -> i32 {
    if let Ok(inp) = fs::read_to_string("inp3.txt") {
	let res = extract_allowed(&inp);
	println!("Day 3 part 2: {}", res);
	res
    } else {
	-999 // really necessary but could learn something at some point
    }
}
