use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader, Lines};
use std::collections::HashSet;

#[derive(PartialEq)]
enum Part {
    One,
    Two,
}

#[derive(Clone)]
enum Op {
    Nop(i32),
    Acc(i32),
    Jmp(i32),
}

#[derive(Clone)]
struct Machine {
    program: Vec<Op>,
    acc: i32,
    pc: usize,
    visited_pc: HashSet<usize>,
}

fn main() -> Result<(), String> {
    let lines = file_lines()?;
    let mut program = parse_program(lines)?;
    println!("Part 1: {}", program.run(Part::One)?);

    let repaired = program.repair()?;
    println!("Part 2: {}", repaired.acc);
    Ok(())
}

impl Machine {
    fn reset(&mut self) {
        self.pc = 0;
        self.acc = 0;
        self.visited_pc = HashSet::new();
    }

    fn run(&mut self, part: Part) -> Result<i32, String> {
        self.reset();
        while let Some(op) = self.program.get(self.pc) {
            match self.visited_pc.get(&self.pc) {
                None => self.visited_pc.insert(self.pc),
                Some(_) if part == Part::One => return Ok(self.acc),
                Some(_) => return Err(String::from("Infinite loop detected."))
            };
            match op {
                Op::Nop(_) => (),
                Op::Acc(operand) => self.acc = self.acc + operand,
                Op::Jmp(operand) => {
                    self.pc = (self.pc as i32 + operand) as usize;
                    continue;
                }
            }
            self.pc = self.pc + 1;
        }
        if self.pc == self.program.len() {
            return Ok(self.acc);
        }
        Err(String::from("PC set to illegal value."))
    }

    fn repair(&self) -> Result<Self, String> {
        let mut reset_self = self.clone();
        reset_self.reset();
        for corrupt_pc in 0..self.program.len() {
            let op = match self.program.get(corrupt_pc) {
                Some(Op::Nop(operand)) => Op::Jmp(*operand),
                Some(Op::Jmp(operand)) => Op::Nop(*operand),
                Some(_) => continue,
                _ => break,
            };
            let mut to_fix = reset_self.clone();
            to_fix.program[corrupt_pc] = op;
            if let Ok(_) = to_fix.run(Part::Two) {
                return Ok(to_fix);
            }
        }
        Err(String::from("Could not repair program."))
    }
}

fn parse_program(lines: Lines<BufReader<File>>) -> Result<Machine, String> {
    let mut program = vec![];
    for line in lines {
        let line = match line {
            Ok(line) => line,
            Err(_) => return Err(String::from("Could not read line.")),
        };
        let operand = match line.get(4..) {
            Some(operand) => match operand.parse::<i32>() {
                Ok(operand) => operand,
                Err(_) => return Err(String::from("Could not parse operand.")),
            },
            None => return Err(String::from("No operand in operation.")),
        };
        let op = match line.get(..3) {
            Some("nop") => Op::Nop(operand),
            Some("acc") => Op::Acc(operand),
            Some("jmp") => Op::Jmp(operand),
            _ => return Err(String::from("Could not parse operation.")),
        };
        program.push(op);
    }
    Ok(Machine {
        program,
        acc: 0,
        pc: 0,
        visited_pc: HashSet::new(),
    })
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