use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader, Lines};

struct Slope { x: usize, y: usize }
const FIRST_SLOPE: Slope = Slope { x: 3, y: 1 };
const SLOPES: [Slope; 5] = [
    Slope { x: 1, y: 1 },
    Slope { x: 3, y: 1 },
    Slope { x: 5, y: 1 },
    Slope { x: 7, y: 1 },
    Slope { x: 1, y: 2 },
];

fn main() -> Result<(), String> {
    let lines = file_lines()?;
    let forest = load_forest(lines)?;

    let trees = count_trees(&FIRST_SLOPE, &forest);
    println!("{} trees will be encountered in part 1.", trees);

    let mut multiple = 1;
    for slope in SLOPES.iter() {
        let trees = count_trees(&slope, &forest);
        multiple = multiple * trees;
    }
    println!("The multiple of all slopes is {}.", multiple);

    Ok(())
}

fn count_trees(slope: &Slope, forest: &Vec<Vec<bool>>) -> u32 {
    let mut counter = 0;
    for y in (slope.y..forest.len()).step_by(slope.y) {
        let x = (y / slope.y) * slope.x;
        let wrapped_x = x % forest[y].len();
        counter = counter + forest[y][wrapped_x] as u32;
    }
    counter
}

fn load_forest(lines: Lines<BufReader<File>>) -> Result<Vec<Vec<bool>>, String> {
    let mut forest = vec![];
    let mut width = None;
    for line in lines {
        let line = match line {
            Ok(line) => line,
            Err(_) => return Err(String::from("Could not read line.")),
        };
        if let Some(width) = width {
            if width != line.len() {
                return Err(String::from("2-D Array has ragged edges."));
            }
        } else {
            width = Some(line.len());
        }
        let mut tree_line: Vec<bool> = vec![];
        for tree_char in line.chars() {
            match tree_char {
                '#' => tree_line.push(true),
                '.' => tree_line.push(false),
                _ => return Err(String::from("Found unexpected character in forest."))
            }
        }
        forest.push(tree_line);
    };
    Ok(forest)
}

fn file_lines() -> Result<Lines<BufReader<File>>, String> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(String::from(
            "Exactly one argument expected (name of file to parse).",
        ));
    }
    let filename = args.get(1).unwrap();
    let file = match File::open(filename) {
        Ok(file) => file,
        Err(_) => return Err(String::from("Could not read input file.")),
    };
    Ok(BufReader::new(file).lines())
}