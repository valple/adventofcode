//use super::helpers::make_grid;
use num::complex::Complex;

fn find_start(grid: &Vec<Vec<char>>) -> Vec<usize> {
    let chrs = vec!['>', 'v','<', '^'];
    let mut ch: char = 'n';
    for i in 0..grid.len() {
	for j in 0..grid[0].len() {
	    ch = grid[i][j];
	    for ind in 0..4 {
		if chrs[ind] == ch {
		    return vec![ind, i, j];
		}
	    } 
	}
    }
    return vec![99, 99, 99];
}

fn part_1() {
    let mut grid = make_grid("./testinp6.txt");
    let [mut dir, mut x, mut y] = find_start(&grid).try_into().unwrap();
    let mut in_grid: bool = true;
    let cdir: Complex<i32> = match dir {
	0 => Complex::new(1, 0),
	1 => Complex::new(0, -1),
	2 => Complex::new(-1, 0),
	3 => Complex::new(0, 1),
	_ => Complex::new(99, 99),
    };
    grid[x][y] = 'X';
    while in_grid {
	x = x - im(cdir);
	y = y + re(cdir);
	if (x >= 0 && x < grid.len() && y >= 0 && y < grid[0].len()) {
	    let ch: char = grid[x][y];
	    if ch == '#' {
		x = x + im(cdir);
		y = y - re(cdir);
		cdir = cdir * (-Complex::I);
	    } else {
		grid[x][y] = 'X';
	    }
	} else {
	    in_grid = false;
	}
    }  	
}
