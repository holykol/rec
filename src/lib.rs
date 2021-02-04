#![feature(test)]
#![feature(str_split_once)]
#![feature(box_syntax)]
#![feature(once_cell)]
#![allow(dead_code)]

mod crypt;
pub use crypt::{decrypt_field, encrypt_field, ENCRYPTED_PREFIX};

mod lex;
mod sx;
use sx::Sx;

mod parser;
use parser::Parser;

use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fs,
    path::Path,
    str::FromStr,
};

use chrono::{DateTime, Utc};
use regex::Regex;
use uuid::Uuid;

pub type Key = String;
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
            _ => Err(format!("unknown constraint: {}", s).into()),
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

#[derive(Debug, Clone, PartialOrd, PartialEq)]
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
    pub rectype: Option<String>,
    pub primary_key: Option<String>,
    pub sort_field: Option<String>,
    pub doc: Option<String>,
    pub types: HashMap<Key, Meta>,
    pub fields: Vec<Key>,
    records: Vec<Record>,
}

type Err = Box<dyn Error>;

impl DB {
    pub fn new(s: &str) -> Result<Self, Err> {
        let tokens = lex::lex(s)?;
        let db = Parser::new().parse(tokens)?;

        Ok(db)
    }

    fn new_from_file(path: &Path) -> Result<Self, Err> {
        let file = fs::read_to_string(path).map_err(|_| "failed to open file")?;
        DB::new(&file)
    }
}

pub struct QueryBuilder<'a> {
    db: &'a DB,
    sx: Option<Sx>,
    sort: Option<String>,
    unique: bool,
}

impl<'a> QueryBuilder<'a> {
    pub fn new(db: &'a DB) -> Self {
        Self {
            db,
            sx: None,
            sort: None,
            unique: false,
        }
    }

    pub fn where_sx(&mut self, s: &str) -> Result<&mut Self, Err> {
        self.sx = Some(Sx::new(s)?);
        Ok(self)
    }

    pub fn sort_by(&mut self, field: &str) -> &mut Self {
        self.sort = Some(field.to_owned());
        self
    }

    pub fn find(&self) -> Result<impl Iterator<Item = &'a Record>, Err> {
        let mut results = Vec::new();

        for rec in &self.db.records {
            if let Some(ref sx) = self.sx {
                if !sx.eval(&rec)? {
                    continue;
                }
            }

            results.push(rec)
        }

        if let Some(ref field) = self.sort.clone().or(self.db.sort_field.clone()) {
            results.sort_by(|x, y| x[field].partial_cmp(&y[field]).unwrap());
        }

        Ok(results.into_iter())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use std::hint::black_box;
    use test::Bencher;

    #[test]
    fn it_works() -> Result<(), Err> {
        let buf = include_str!("/home/t/dev/rust/rec/src/test.rec");
        let db = DB::new(buf).unwrap();

        let result = QueryBuilder::new(&db)
            .where_sx("Login = 'foo'")?
            .sort_by("Name")
            .find()?;

        assert_eq!(result.count(), 1);
        Ok(())
    }

    #[test]
    fn parser_records() {
        let db = DB::new("hello: world\nblah: blah\n\nhello: mom\nblah: bruh").unwrap();
        assert_eq!(db.records.len(), 2);
    }

    #[bench]
    fn bench(b: &mut Bencher) {
        let buf = include_str!("/home/t/dev/rust/rec/src/test.rec");
        b.iter(|| DB::new(black_box(buf)).unwrap());
    }
}
