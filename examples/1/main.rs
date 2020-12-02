use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader, Lines};

const GOAL: u32 = 2020;

fn main() -> Result<(), String> {
    let lines = file_lines()?;
    let numbers = lines.map(|line| {
        let line = match line {
            Ok(line) => line,
            Err(_) => return Err(String::from("Could not read line!")),
        };
        match line.parse::<u32>() {
            Ok(n) => Ok(n),
            Err(_) => Err(String::from("Could not parse input line!")),
        }
    });

    let mut n_map = HashMap::new();
    for n in numbers {
        let n = n?;
        if n_map.contains_key(&n) {
            n_map.insert(n, n_map.get(&n).unwrap() + 1);
        } else {
            n_map.insert(n, 1);
        }
    }

    match find_tuples(GOAL, &n_map) {
        Ok((n_1, n_2)) => {
            println!("{} + {} = {}", n_1, n_2, n_1 + n_2);
            println!("{} * {} = {}", n_1, n_2, n_1 * n_2);
        }
        Err(err) => eprintln!("{}", err),
    }

    match find_triplets(GOAL, &n_map) {
        Ok((n_1, n_2, n_3)) => {
            println!("{} + {} + {} = {}", n_1, n_2, n_3, n_1 + n_2 + n_3);
            println!("{} * {} * {} = {}", n_1, n_2, n_3, n_1 * n_2 * n_3);
        }
        Err(err) => eprintln!("{}", err),
    }
    Ok(())
}

fn find_tuples(goal: u32, n_map: &HashMap<u32, u32>) -> Result<(u32, u32), String> {
    for (&n_1, &repeats) in n_map.iter() {
        let n_2 = match goal.checked_sub(n_1) {
            Some(n_2) => n_2,
            None => continue,
        };
        if n_1 == n_2 && repeats >= 2 || n_1 != n_2 && n_map.contains_key(&n_2) {
            return Ok((n_1, n_2));
        }
    }
    Err(String::from("Could not find sought number tuple!"))
}

fn find_triplets(goal: u32, n_map: &HashMap<u32, u32>) -> Result<(u32, u32, u32), String> {
    for (&n_1, &repeats) in n_map.iter() {
        let missing = match goal.checked_sub(n_1) {
            Some(missing) => missing,
            None => continue,
        };
        let (n_2, n_3) = match find_tuples(missing, n_map) {
            Ok((n_2, n_3)) => (n_2, n_3),
            Err(_) => continue,
        };
        if n_1 == n_2 && n_2 == n_3 && repeats >= 3
            || (n_1 == n_2 || n_1 == n_3) && repeats >= 2
            || n_1 != n_2 && n_1 != n_3
        {
            return Ok((n_1, n_2, n_3));
        }
    }
    Err(String::from("Could not find sought number triplet!"))
}

fn file_lines() -> Result<Lines<BufReader<File>>, String> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(String::from(
            "Exactly one argument expected (name of file to parse)!",
        ));
    }
    let filename = args.get(1).unwrap();
    let file = match File::open(filename) {
        Ok(file) => file,
        Err(_) => return Err(String::from("Could not read input file!")),
    };
    Ok(BufReader::new(file).lines())
}
