#![feature(str_split_once)]
#![feature(box_syntax)]
#![feature(once_cell)]
#![allow(dead_code)]
#![allow(unused_variables)]

mod lex;
use crate::lex::*;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::path::Path;

use regex::Regex;

type Key = String;

#[derive(Default)]
pub struct DB {
    rectype: Option<String>,
    primary_key: Option<String>,
    doc: Option<String>,
    types: HashMap<Key, Meta>,
    records: Vec<Record>,
}

pub type Record = HashMap<Key, Field>;

pub struct Field {
    name: Key,
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
    Allowed,
    Mandatory,
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
    Regexp(Box<Regex>),
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
        use crate::lex::Token::*;

        for token in tokens.iter() {
            println!("token: {:?}", token);
            match token {
                Keyword(keyword, value) => match keyword.as_ref() {
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
                Blank => unimplemented!(),
                _ => println!("unkown token {:?}", token),
            }
        }

        Ok(())
    }
}

fn parse_type(args: Vec<&str>) -> Result<FieldKind, Box<dyn Error>> {
    let &tt = args.get(1).unwrap();

    match tt {
        "line" => Ok(FieldKind::Line),
        "int" => Ok(FieldKind::Int),
        "real" => Ok(FieldKind::Real),
        "bool" => Ok(FieldKind::Bool),
        "date" => Ok(FieldKind::Date),
        "email" => Ok(FieldKind::Date),
        "field" => Ok(FieldKind::Field),
        "regexp" => {
            let rx = args.get(2).unwrap();
            let compiled = Regex::new(rx)?;
            Ok(FieldKind::Regexp(box compiled))
        }
        "uuid" => Ok(FieldKind::UUID),
        "viz" => {
            // TODO validate field
            unimplemented!()
        }
        "enum" => {
            unimplemented!()
        }
        _ => Err("unknown type value".into()),
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    #[ignore = "come back in around 20 years"]
    fn it_works() {
        //let path = Path::new("/home/t/dev/rust/rec/src/test.rec");
        //let _db = DB::new(&path).unwrap();
    }

    #[test]
    fn parser_rec_type() {
        let db = DB::new("%rec: Book".to_owned()).unwrap();
        assert_eq!(db.rectype, Some("Book".to_owned()));
    }

    #[test]
    fn parser_field_type() {
        let db = DB::new("%type: Book line".to_owned()).unwrap();
        let meta = db.types.get(&"Book".to_owned()).unwrap();
        match meta.kind.as_ref().unwrap() {
            FieldKind::Line => (),
            _ => panic!("Field type should be line"),
        }
    }
}
