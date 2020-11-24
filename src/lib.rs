#![feature(str_split_once)]
#![feature(box_syntax)]
#![feature(once_cell)]
#![allow(dead_code)]
#![allow(unused_variables)]

mod lex;
use crate::lex::{lex, Token};

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::path::Path;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref FIELD_RX: Regex = Regex::new("[a-zA-Z%][a-zA-Z0-9_]*").unwrap();
    static ref ENUM_RX: Regex = Regex::new("[a-zA-Z0-9][a-zA-Z0-9_-]*").unwrap();
}

type Key = String;

#[derive(Default, Debug)]
pub struct DB {
    rectype: Option<String>,
    primary_key: Option<String>,
    doc: Option<String>,
    types: HashMap<Key, Meta>,
    records: Vec<Record>,
}

pub type Record = HashMap<Key, Field>;

#[derive(Debug)]
pub struct Field {
    value: String,
    kind: FieldKind,
}

#[derive(Debug, Default)]
pub struct Meta {
    unique: bool,
    constraint: Option<Constraint>,
    kind: Option<FieldKind>,
}

#[derive(Debug)]
enum Constraint {
    Mandatory,
    Allowed,
    Phohibit,
}

#[derive(Debug)]
pub enum FieldKind {
    Line,
    Int,
    Real,
    Bool,
    Date,
    Email,
    Field,
    UUID,
    Regexp(Regex),
    Viz(String),
    Enum(HashSet<String>),
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
        let mut current_record: Option<Record> = None;

        for token in tokens.iter() {
            println!("token: {:?}", token);
            match token {
                Token::Keyword(keyword, value) => match keyword.as_ref() {
                    "rec" => self.rectype = Some(value.clone()),
                    "type" => {
                        let args: Vec<&str> = value.split_whitespace().collect();
                        let field_name = args.get(0).unwrap().to_string();
                        let kind = parse_type(args)?;

                        if !self.types.contains_key(&field_name) {
                            self.types.insert(field_name.clone(), Meta::default());
                        }

                        let meta = self.types.get_mut(&field_name).unwrap();
                        meta.kind = Some(kind)
                    }
                    _ => (),
                },
                Token::Field(key, value) => {
                    if current_record.is_none() {
                        current_record = Some(Default::default())
                    }

                    match current_record {
                        Some(ref mut rec) => {
                            let f = Field {
                                value: value.clone(),
                                kind: FieldKind::Line,
                            };

                            rec.insert(key.clone(), f);
                        }
                        None => unreachable!(),
                    }
                }

                Token::Blank => {
                    if let Some(rec) = current_record.take() {
                        records.push(rec);
                    }
                }
            }
        }

        if let Some(rec) = current_record {
            records.push(rec);
        }

        self.records = records;

        Ok(())
    }
}

fn parse_type(args: Vec<&str>) -> Result<FieldKind, Box<dyn Error>> {
    use FieldKind::*;

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
        "email" => Ok(Date),
        "field" => Ok(Field),
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
                true => Ok(FieldKind::Viz(key.to_string())),
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
    #[ignore = "come back in around 20 years"]
    fn it_works() {
        let buf = include_str!("/home/t/dev/rust/rec/src/test.rec");
        let db = DB::new(buf.to_string()).unwrap();
        println!("{:?}", db);
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
        assert!(matches!(meta.kind, Some(FieldKind::Line)))
    }

    #[test]
    fn parser_type_regex() {
        let db = DB::new("%type: Phone regexp /^[0-9]{10}$/".to_owned()).unwrap();
        let meta = db.types.get(&"Phone".to_owned()).unwrap();

        if let Some(FieldKind::Regexp(ref rx)) = meta.kind {
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

        if let Some(FieldKind::Enum(ref variants)) = meta.kind {
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
