#![feature(test)]
#![feature(str_split_once)]
#![feature(box_syntax)]
#![feature(once_cell)]
#![allow(dead_code)]

mod crypt;
pub use crypt::{decrypt_field, encrypt_field, ENCRYPTED_PREFIX};

mod lex;
mod sx;

mod parser;
use parser::Parser;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::path::Path;
use std::str::FromStr;

use chrono::{DateTime, Utc};
use regex::Regex;
use uuid::Uuid;

type Key = String;
pub type Record = HashMap<Key, Value>;

#[derive(Debug, Default)]
pub struct Meta {
    unique: bool,
    constraint: Option<Constraint>,
    kind: Kind,
}

#[derive(Debug)]
enum Constraint {
    Mandatory,
    Allowed,
    Prohibited,
}

impl FromStr for Constraint {
    type Err = Err;

    // so we don't pollute parser code
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "mandatory" => Ok(Constraint::Mandatory),
            "allowed" => Ok(Constraint::Allowed),
            "prohibited" => Ok(Constraint::Prohibited),
            _ => Err(format!("unknown constraint: {}", s))?,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Kind {
    Line, // TODO size
    Int,
    Real,
    Bool,
    Date,
    Email,
    UUID,
    Confidential,
    Range(isize, isize),
    Regexp(Regex),
    Viz(String),
    Enum(HashSet<String>),
}

impl Default for Kind {
    fn default() -> Self {
        Kind::Line
    }
}

#[derive(Debug)]
pub enum Value {
    Line(String),
    Int(isize),
    Real(f64),
    Bool(bool),
    Date(DateTime<Utc>),
    Email(String),
    UUID(Uuid),
    Confidential(String),
    Range(isize),
    Regexp(String),
    Viz(String),
    Enum(String),
}

#[derive(Default, Debug)]
pub struct DB {
    rectype: Option<String>,
    primary_key: Option<String>,
    doc: Option<String>,
    types: HashMap<Key, Meta>,
    records: Vec<Record>,
}

type Err = Box<dyn Error>;

impl DB {
    fn new(s: &str) -> Result<Self, Err> {
        let tokens = lex::lex(s)?;
        let db = Parser::new().parse(tokens)?;

        Ok(db)
    }

    fn new_from_file(path: &Path) -> Result<Self, Err> {
        let file = fs::read_to_string(path).map_err(|_| "failed to open file")?;
        DB::new(&file)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    #[ignore = "come back in around 20 years"]
    fn it_works() {
        let buf = include_str!("/home/t/dev/rust/rec/src/test.rec");
        let db = DB::new(buf).unwrap();
        println!("{:?}", db);
        todo!();
    }

    #[test]
    fn parser_records() {
        let db = DB::new("hello: world\nblah: blah\n\nhello: mom\nblah: bruh").unwrap();
        assert_eq!(db.records.len(), 2);
    }
}
