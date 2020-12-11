use std::collections::{VecDeque, HashMap};
use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::env;

const BUFFER_SIZE: usize = 25;

fn main() -> Result<(), String> {
    let numbers = numbers()?;

    let invalid = find_invalid(&numbers, BUFFER_SIZE)?;
    println!("Part 1: {}", invalid);

    let range = find_range(invalid, &numbers)?;
    let min = match range.iter().min() {
        Some(min) => min,
        None => return Err("Could not find minimum of empty Vec.".to_string()),
    };
    let max = *range.iter().max().unwrap();
    println!("Part 2: {:?}", min + max);

    Ok(())
}

fn find_invalid(numbers: &Vec<u64>, buffer_size: usize) -> Result<u64, String> {
    let mut buffer = Buffer::new();
    let mut numbers_iter = numbers.iter();

    // Fill the buffer.
    for _ in 0..buffer_size {
        if let Some(&number) = numbers_iter.next() {
            buffer.push(number);
        } else {
            return Err("Not enough numbers to fill buffer.".to_string())
        }
    }

    // Find invalid number.
    for &number in numbers_iter {
        let mut pair = None;
        for cand_1 in buffer.iter() {
            if let Some(rest) = number.checked_sub(*cand_1) {
                if buffer.contains(rest) {
                    pair = Some((cand_1, rest));
                    break;
                }
            }
        }
        if pair.is_none() {
            return Ok(number);
        }
        buffer.push(number);
    }
    Err("Could not find invalid number.".to_string())
}

fn find_range(goal: u64, numbers: &Vec<u64>) -> Result<Vec<u64>, String> {
    let mut range = VecDeque::new();

    for &number in numbers.iter() {
        range.push_back(number);
        let mut sum = range.iter().sum::<u64>();
        while sum > goal {
            range.pop_front();
            sum = range.iter().sum::<u64>();
        }
        if sum == goal {
            return Ok(range.into());
        }
    }
    Ok(vec![])
}

fn numbers() -> Result<Vec<u64>, String> {
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
    let mut numbers = vec![];
    for line in BufReader::new(file).lines() {
        match line {
            Ok(line) => match line.parse() {
                Ok(num) => numbers.push(num),
                Err(err) => {
                    println!("{}", err);
                    return Err("Could not parse number.".to_string())
                },
            },
            _ => return Err("Failed to read line.".to_string()),
        };
    }
    Ok(numbers)
}
struct Buffer {
    map: HashMap<u64, u32>,
    queue: VecDeque<u64>,
}

struct BufferIter<'a> {
    buffer: &'a Buffer,
    curr: usize,
    next: usize,
}

impl Buffer {
    fn new() -> Self {
        Buffer {
            map: HashMap::with_capacity(BUFFER_SIZE),
            queue: VecDeque::with_capacity(BUFFER_SIZE),
        }
    }

    fn push(&mut self, num: u64) {
        self.queue.push_back(num);
        match self.map.get(&num) {
            Some(&val) => self.map.insert(num, val + 1),
            None => self.map.insert(num, 0),
        };
        if self.queue.len() > BUFFER_SIZE {
            let old_num = self.queue.pop_front().unwrap();
            match self.map.get(&old_num) {
                Some(&val) if val > 0 => self.map.insert(old_num, val - 1),
                Some(_) => self.map.remove(&old_num),
                None => panic!(),
            };
        }
    }

    fn contains(&self, num: u64) -> bool {
        self.map.contains_key(&num)
    }

    fn iter(&self) -> BufferIter {
        BufferIter {
            buffer: self,
            curr: 0,
            next: 0,
        }
    }
}

impl<'a> Iterator for BufferIter<'a> {
    type Item = &'a u64;
    
    fn next(&mut self) -> Option<&'a u64> {
        self.curr += 1;
        self.next += 1;
        self.buffer.queue.get(self.curr)
    }
}