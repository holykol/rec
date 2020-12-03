use super::{Err, Kind, Record, Value, DB};
use crate::crypt;
use crate::lex::Token;
use std::collections::HashSet;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref FIELD_RX: Regex = Regex::new("[a-zA-Z%][a-zA-Z0-9_]*").unwrap();
    static ref ENUM_RX: Regex = Regex::new("[a-zA-Z0-9][a-zA-Z0-9_-]*").unwrap();
}

#[derive(Debug, Default)]
pub(crate) struct Parser {
    db: DB,
    current_record: Option<Record>,
}

impl Parser {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn parse(mut self, tokens: Vec<Token>) -> Result<DB, Err> {
        let mut records = Vec::new();

        for token in tokens.iter() {
            println!("token: {:?}", token);
            match token {
                Token::Keyword(keyword, value) => {
                    let args = value.split_whitespace().collect();
                    self.parse_keyword(keyword, args)?
                }
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

        self.db.records = records;

        Ok(self.db)
    }

    fn parse_keyword(&mut self, key: &str, args: Vec<&str>) -> Result<(), Err> {
        match key {
            "rec" => self.db.rectype = Some(args.get(0).ok_or("expected rec type")?.to_string()),
            "key" => self.db.primary_key = Some(args.get(0).ok_or("expected key")?.to_string()),
            "doc" => self.db.doc = Some(args.join(" ")),
            "type" => {
                let field_name = args.get(0).ok_or("expected field name")?.to_string();
                let kind = parse_type(args)?;

                let meta = self.db.types.entry(field_name).or_default();
                meta.kind = kind;
            }
            "confidential" => {
                for field in args {
                    let meta = self.db.types.entry(field.to_owned()).or_default();
                    if !matches!(meta.kind, Kind::Line) {
                        return Err("confidential fields are always lines".into());
                    }
                    meta.kind = Kind::Confidential;
                }
            }
            "mandatory" | "allowed" | "prohibited" => {
                for field in args {
                    let meta = self.db.types.entry(field.to_owned()).or_default();
                    meta.constraint = Some(key.parse()?);
                }
            }
            "unique" => {
                for field in args {
                    let meta = self.db.types.entry(field.to_owned()).or_default();
                    meta.unique = true
                }
            }

            key => todo!("{}", key),
        };

        Ok(())
    }

    fn parse_field(&mut self, key: &str, value: &str) -> Result<(), Err> {
        let mut rec = self.current_record.take().unwrap_or_default();

        let meta = self.db.types.entry(key.to_owned()).or_default();
        println!("{:?}", meta);
        let val = parse_value(&meta.kind, value)?;

        rec.insert(key.to_owned(), val);

        // put it back
        self.current_record = Some(rec);
        Ok(())
    }
}

fn parse_value(kind: &Kind, val: &str) -> Result<Value, Err> {
    use Value::*;

    Ok(match kind {
        Kind::Line => Line(val.to_owned()),
        Kind::Int => Int(val.parse()?),
        Kind::Real => Real(val.parse()?),
        Kind::Bool => match val {
            "true" | "yes" | "1" => Bool(true),
            "false" | "no" | "0" => Bool(false),
            _ => return Err(format!("unexpected boolean value: {}", val).into()),
        },
        Kind::Date => todo!("date parsing"),
        Kind::Email => {
            if !val.contains('@') {
                // yes
                return Err(format!("invalid email adress: {}", val).into());
            }
            Email(val.to_owned())
        }
        Kind::UUID => UUID(val.parse()?),
        Kind::Confidential => {
            if !val.starts_with(crypt::ENCRYPTED_PREFIX) {
                return Err("possibly unencrypted confidential value".into());
            }
            Confidential(val.to_owned())
        }
        Kind::Range(min, max) => {
            let n = val.parse()?;
            if n < *min || n > *max {
                return Err("value is out of range".into());
            }
            Range(n)
        }
        Kind::Regexp(rx) => {
            if !rx.is_match(val) {
                return Err(format!("{} does not match required format", val).into());
            }
            Regexp(val.to_owned())
        }
        Kind::Viz(_) => Viz(val.to_owned()), // We can't validate that other database has the key
        Kind::Enum(variants) => {
            if !variants.contains(&val.to_lowercase()) {
                return Err(format!("invalid enum value: {}", val).into());
            }
            Enum(val.to_lowercase())
        }
    })
}

fn parse_type(args: Vec<&str>) -> Result<Kind, Err> {
    use Kind::*;

    let &tt = args.get(1).ok_or("expected field name")?;

    if !FIELD_RX.is_match(&tt) {
        return Err(format!("invalid field name: {}", tt).into());
    }

    Ok(match tt {
        "line" => Line,
        "int" => Int,
        "real" => Real,
        "bool" => Bool,
        "date" => Date,
        "email" => Email,
        "uuid" => UUID,
        "range" => {
            let from;
            let to;

            if args.len() > 3 {
                from = parse_bound(args.get(2).ok_or("expected start range index")?)?;
                to = parse_bound(args.get(3).ok_or("expected end range index")?)?;
            } else {
                from = 0;
                to = parse_bound(args.get(2).ok_or("expected end range index")?)?;
            }

            if from > to {
                return Err("impossible range".into());
            }

            Kind::Range(from, to)
        }
        "regexp" => {
            let rx = args
                .get(2)
                .ok_or("expected regexp definition as third field")?
                .strip_prefix("/")
                .ok_or("expected regexp to begin with slash")?
                .strip_suffix("/")
                .ok_or("expected regexp to end with slash")?;

            let compiled = Regex::new(rx)?;
            Regexp(compiled)
        }
        "viz" => {
            let key = args
                .get(2)
                .ok_or("expected field reference as second field")?;

            match FIELD_RX.is_match(key) {
                true => Kind::Viz(key.to_string()),
                false => return Err(format!("invalid viz value: {}", key).into()),
            }
        }
        "enum" => {
            if args.len() < 3 {
                return Err("expected at least one enum variant".into());
            }

            let mut variants = HashSet::with_capacity(args.len() - 2);

            for v in args.iter().skip(2).map(|s| s.to_string()) {
                match ENUM_RX.is_match(&v) {
                    true => variants.insert(v.to_lowercase()),
                    false => return Err(format!("invalid enum value: {}", v).into()),
                };
            }

            Enum(variants)
        }
        _ => return Err(format!("unknown type: {}", tt).into()),
    })
}

fn parse_bound(v: &str) -> Result<isize, Err> {
    Ok(match v {
        "MIN" => isize::MIN,
        "MAX" => isize::MAX,
        v => v.parse()?,
    })
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::Meta;

    #[test]
    fn parser_rec_type() {
        let db = DB::new("%rec: Book").unwrap();
        assert_eq!(db.rectype, Some("Book".to_owned()));
    }

    #[test]
    fn parser_type_line() {
        let db = DB::new("%type: Book line").unwrap();
        let meta = db.types.get(&"Book".to_owned()).unwrap();
        assert!(matches!(meta.kind, Kind::Line))
    }

    #[test]
    fn parser_regex() {
        let db = DB::new("%type: Phone regexp /^[0-9]{10}$/").unwrap();
        let meta = db.types.get(&"Phone".to_owned()).unwrap();

        assert!(matches!(meta.kind, Kind::Regexp(_)));

        assert!(parse_value(&meta.kind, &"0123456789".to_owned()).is_ok());
        assert!(parse_value(&meta.kind, &"blah".to_owned()).is_err());
    }

    #[test]
    fn parser_enum() {
        let db = DB::new("%type: Status enum Loading Done Error").unwrap();
        let meta = db.types.get(&"Status".to_owned()).unwrap();

        if let Kind::Enum(ref variants) = meta.kind {
            assert_eq!(variants.len(), 3);
            assert!(variants.contains("loading"));
            assert!(variants.contains("done"));
            assert!(variants.contains("error"));
        } else {
            panic!("unexpected type")
        }

        assert!(parse_value(&meta.kind, &"Done".to_owned()).is_ok());
        assert!(parse_value(&meta.kind, &"blah".to_owned()).is_err());
    }

    #[test]
    fn parser_range() {
        let kind = parse_type(vec!["field", "range", "MIN", "MAX"]).unwrap();
        assert!(matches!(kind, Kind::Range(isize::MIN, isize::MAX)));

        let kind = parse_type(vec!["field", "range", "0", "15"]).unwrap();
        assert!(matches!(kind, Kind::Range(0, 15)));

        let kind = parse_type(vec!["field", "range", "15"]).unwrap();
        assert!(matches!(kind, Kind::Range(0, 15)));

        // Impossible range
        assert!(parse_type(vec!["field", "range", "30", "15"]).is_err());
    }

    #[test]
    fn parser_confidential() {
        let db = DB::new("%confidential: Password Token").unwrap();

        assert!(matches!(db.types.get("Password").unwrap(), Meta { kind: Kind::Confidential, .. }));
        assert!(matches!(db.types.get("Token").unwrap(), Meta { kind: Kind::Confidential, .. }));

        assert!(matches!(
            parse_value(&Kind::Confidential, "encrypted-AABBCC").unwrap(),
            Value::Confidential(_)
        ));
        assert!(parse_value(&Kind::Confidential, "blah").is_err());
    } //
}
