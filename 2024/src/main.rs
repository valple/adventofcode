use std::thread;
use std::time;

//use helpers;
//use crate::day1;
// use crate::day2;
mod helpers;
mod day1;
mod day2;
mod day3;
mod day6;


fn main() {
    let ten_millis = time::Duration::from_millis(5000);
    // if let Ok(lines) = read_lines("./test.txt") {
    // 	for line in lines {
    // 	    println!("{}", line.unwrap());
    // 	    thread::sleep(ten_millis);
    // 	}
    // } else {
    // 	println!("Error error error");
    // 	thread::sleep(ten_millis);
    // }
    let a = day1::part_1();
    println!("Day 1 part 1: {}", a);
    let d2_1 = day2::part_1();
    println!("Day 2 part 1: {}", d2_1);
    let d2_2 = day2::part_2();
    println!("Day 2 part 2: {}", d2_2);
    day3::part_1();
    day3::part_2();
    thread::sleep(ten_millis);
}

// -2779145
