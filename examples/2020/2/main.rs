use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader, Lines};

fn main() -> Result<(), String> {
    let lines = file_lines()?;
    let valid = count_valid_passwords_old(lines)?;
    println!(
        "There are {} valid passwords according to the old policy.",
        valid
    );

    let lines = file_lines()?;
    let valid = count_valid_passwords_new(lines)?;
    println!(
        "There are {} valid passwords according to the new policy.",
        valid
    );
    Ok(())
}

fn count_valid_passwords_new(mut lines: Lines<BufReader<File>>) -> Result<u32, String> {
    match lines.next() {
        Some(Ok(line)) => {
            let (_, (lower, upper, tag, pwd)) = parser(&line[..])?;
            let (lower, upper) = (lower as usize, upper as usize);

            if pwd.len() >= lower && pwd.len() >= upper && lower > 0 && upper > 0 {
                let lower_eq = parse_tag(tag)(&pwd[(lower - 1)..]).is_ok();
                let upper_eq = parse_tag(tag)(&pwd[(upper - 1)..]).is_ok();
                let is_valid = lower_eq != upper_eq;
                Ok(is_valid as u32 + count_valid_passwords_new(lines)?)
            } else {
                Err(String::from("Character positions out of bounds"))
            }
        }
        None => Ok(0),
        Some(Err(_)) => Err(String::from("Could not read line!")),
    }
}

fn count_valid_passwords_old(mut lines: Lines<BufReader<File>>) -> Result<u32, String> {
    match lines.next() {
        Some(Ok(line)) => {
            let (_, (lower, upper, tag, password)) = parser(&line[..])?;
            let tags = count_tags(tag)(password);
            let is_valid = lower <= tags && tags <= upper;

            Ok(is_valid as u32 + count_valid_passwords_old(lines)?)
        }
        None => Ok(0),
        Some(Err(_)) => Err(String::from("Could not read line!")),
    }
}

fn count_tags<'a>(tag: &'a str) -> impl Fn(&'a str) -> u32 {
    move |input: &str| {
        if let Ok((input, _)) = parse_tag(tag)(input) {
            1 + count_tags(tag)(input)
        } else if let Some(first_char) = input.chars().next() {
            let first_char_len = first_char.to_string().len();
            count_tags(tag)(&input[first_char_len..])
        } else {
            0
        }
    }
}

fn parser(input: &str) -> Result<(&str, (u32, u32, &str, &str)), String> {
    let (input, lower) = parse_digit1(input)?;
    let lower = lower.parse::<u32>().unwrap();
    let (input, _) = parse_tag("-")(input)?;
    let (input, upper) = parse_digit1(input)?;
    let upper = upper.parse::<u32>().unwrap();
    let (input, _) = parse_tag(" ")(input)?;
    let (input, tag) = parse_alpha1(input)?;
    let (input, _) = parse_tag(": ")(input)?;
    let (input, password) = parse_alpha1(input)?;
    Ok((input, (lower, upper, tag, password)))
}

fn parse_alpha1(input: &str) -> Result<(&str, &str), String> {
    match parse_while1(|c| c.is_alphabetic())(input) {
        Ok((input, token)) => Ok((input, token)),
        Err(_) => Err(String::from("Could not find alphabetic characters.")),
    }
}

fn parse_digit1(input: &str) -> Result<(&str, &str), String> {
    match parse_while1(|c| c.is_ascii_digit())(input) {
        Ok((input, token)) => Ok((input, token)),
        Err(_) => Err(String::from("Could not find numeric characters.")),
    }
}

fn parse_tag<'a>(tag: &'a str) -> impl Fn(&'a str) -> Result<(&'a str, &'a str), String> {
    move |input: &str| {
        if let Some(input_slice) = input.get(..tag.len()) {
            if *tag == *input_slice {
                return Ok((&input[tag.len()..], &input[..tag.len()]));
            }
        } else if tag.len() == 0 {
            return Ok((input, &input[0..0]));
        }
        Err(format!("Could not find tag {} in input", tag))
    }
}

fn parse_while1<'a>(
    test: impl Fn(char) -> bool,
) -> impl FnOnce(&'a str) -> Result<(&'a str, &'a str), String> {
    move |input: &str| {
        if let Some(input_char) = input.chars().next() {
            if test(input_char) {
                let char_len = input_char.to_string().len();

                if let Ok((_, token)) = parse_while1(test)(&input[char_len..]) {
                    let token_len = token.len() + char_len;
                    return Ok((&input[token_len..], &input[..token_len]));
                } else {
                    return Ok((&input[char_len..], &input[..char_len]));
                }
            }
        }
        Err(String::from(
            "Could not find character satisfying the test.",
        ))
    }
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
