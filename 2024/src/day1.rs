use super::helpers::read_lines;
fn parse_line(line: &str) -> i32 {
    // let a =
    line.split_whitespace()
	.map(|x| x.parse::<i32>().unwrap())
	.reduce(|x, y| y - x)
	.unwrap()
    //;
    //  println!("{}", a);
	// return a;
}
pub fn part_1() -> i32 {
    let res: i32;
    if let Ok(lines) = read_lines("./inp1.txt") {
	res = lines.flatten()
	    .map(|x| parse_line(&x))
	    .fold(0, |x, y| x + y);
    } else {
	    res = -99;
    }
    return res;
}

