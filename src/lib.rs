#![feature(str_split_once)]
#![feature(box_syntax)]
#![feature(once_cell)]
#![allow(dead_code)]
#![allow(unreachable_code)]
#![allow(unused_variables)]

mod lex;
use crate::lex::{lex, Token};

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::path::Path;

use chrono::{DateTime, Utc};
use lazy_static::lazy_static;
use regex::Regex;
use uuid::Uuid;

lazy_static! {
    static ref FIELD_RX: Regex = Regex::new("[a-zA-Z%][a-zA-Z0-9_]*").unwrap();
    static ref ENUM_RX: Regex = Regex::new("[a-zA-Z0-9][a-zA-Z0-9_-]*").unwrap();
}

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
    Phohibit,
}

#[derive(Debug, Clone)]
pub enum Kind {
    Line,
    Int,
    Real,
    Bool,
    Date,
    Email,
    UUID,
    Confidential,
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
    //
    current_record: Option<Record>,
}

impl DB {
    fn new(s: String) -> Result<Self, Box<dyn Error>> {
        let mut db = DB::default();
        let tokens = lex(s)?;

        db.parse(tokens)?;
        Ok(db)
    }
    fn new_from_file(path: &Path) -> Result<Self, Box<dyn Error>> {
        let file = fs::read_to_string(path).map_err(|_| "failed to open file")?;
        DB::new(file)
    }

    fn parse(&mut self, tokens: Vec<Token>) -> Result<(), Box<dyn Error>> {
        let mut records = Vec::new();

        for token in tokens.iter() {
            println!("token: {:?}", token);
            match token {
                Token::Keyword(keyword, value) => self.parse_keyword(keyword, value)?,
                Token::Field(key, value) => self.parse_field(key, value)?,
                Token::Blank => {
                    if let Some(rec) = self.current_record.take() {
                        records.push(rec);
                    }
                }
            }
        }

        if let Some(rec) = self.current_record.take() {
            records.push(rec);
        }

        self.records = records;

        Ok(())
    }

    fn parse_keyword(&mut self, key: &String, value: &String) -> Result<(), Box<dyn Error>> {
        match key.as_ref() {
            "rec" => self.rectype = Some(value.clone()),
            "type" => {
                let args: Vec<&str> = value.split_whitespace().collect();
                let field_name = args.get(0).ok_or("expected field name")?.to_string();
                let kind = parse_type(args)?;

                let meta = self.types.entry(field_name).or_default();
                meta.kind = kind;
            }
            key => println!("unimplemented: {}", key),
        };

        Ok(())
    }

    fn parse_field(&mut self, key: &String, value: &String) -> Result<(), Box<dyn Error>> {
        let mut rec = self.current_record.take().unwrap_or_default();

        let mut kind = Kind::default();
        if let Some(meta) = self.types.get(key) {
            kind = meta.kind.clone();
        }

        println!("{:?}", kind);

        rec.insert(key.clone(), Value::Line(value.clone()));

        // put it back
        self.current_record = Some(rec);
        Ok(())
    }
}

fn parse_type(args: Vec<&str>) -> Result<Kind, Box<dyn Error>> {
    use Kind::*;

    let &tt = args.get(1).ok_or("expected field name")?;

    if !FIELD_RX.is_match(&tt) {
        return Err(format!("invalid field name: {}", tt).into());
    }

    match tt {
        "line" => Ok(Line),
        "int" => Ok(Int),
        "real" => Ok(Real),
        "bool" => Ok(Bool),
        "date" => Ok(Date),
        "email" => Ok(Email),
        "uuid" => Ok(UUID),
        "regexp" => {
            let rx = args
                .get(2)
                .ok_or("expected regexp definition as third field")?
                .strip_prefix("/")
                .ok_or("expected regexp to begin with slash")?
                .strip_suffix("/")
                .ok_or("expected regexp to end with slash")?;

            let compiled = Regex::new(rx)?;
            Ok(Regexp(compiled))
        }
        "viz" => {
            let key = args
                .get(2)
                .ok_or("expected field reference as second field")?;

            match FIELD_RX.is_match(key) {
                true => Ok(Kind::Viz(key.to_string())),
                false => Err(format!("invalid viz value: {}", key).into()),
            }
        }
        "enum" => {
            if args.len() < 3 {
                return Err("expected at least one enum variant".into());
            }

            let mut variants = HashSet::with_capacity(args.len() - 2);

            for v in args.iter().skip(2).map(|s| s.to_string()) {
                match ENUM_RX.is_match(&v) {
                    true => variants.insert(v),
                    false => return Err(format!("invalid enum value: {}", v).into()),
                };
            }

            Ok(Enum(variants))
        }
        _ => Err(format!("unknown type: {}", tt).into()),
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    //#[ignore = "come back in around 20 years"]
    fn it_works() {
        let buf = include_str!("/home/t/dev/rust/rec/src/test.rec");
        let db = DB::new(buf.to_string()).unwrap();
        println!("{:?}", db);
        todo!()
    }

    #[test]
    fn parser_rec_type() {
        let db = DB::new("%rec: Book".to_owned()).unwrap();
        assert_eq!(db.rectype, Some("Book".to_owned()));
    }

    #[test]
    fn parser_type_line() {
        let db = DB::new("%type: Book line".to_owned()).unwrap();
        let meta = db.types.get(&"Book".to_owned()).unwrap();
        assert!(matches!(meta.kind, Kind::Line))
    }

    #[test]
    fn parser_type_regex() {
        let db = DB::new("%type: Phone regexp /^[0-9]{10}$/".to_owned()).unwrap();
        let meta = db.types.get(&"Phone".to_owned()).unwrap();

        if let Kind::Regexp(ref rx) = meta.kind {
            assert!(rx.is_match("0123456789"));
            assert!(!rx.is_match("blah"));
            return;
        }
        panic!("unexpected type")
    }

    #[test]
    fn parser_type_enum() {
        let db = DB::new("%type: Status enum Loading Done Error".to_owned()).unwrap();
        let meta = db.types.get(&"Status".to_owned()).unwrap();

        if let Kind::Enum(ref variants) = meta.kind {
            assert_eq!(variants.len(), 3);
            assert!(variants.contains("Loading"));
            assert!(variants.contains("Done"));
            assert!(variants.contains("Error"));
            return;
        }
        panic!("unexpected type")
    }

    #[test]
    fn parser_records() {
        let db = DB::new("hello: world\nblah: blah\n\nhello: mom\nblah: bruh".to_owned()).unwrap();

        assert_eq!(db.records.len(), 2);
    }
}
