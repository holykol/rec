use std::iter::{Iterator, Peekable};

#[derive(Debug, PartialEq)]
pub(crate) enum Token {
    Keyword(String, String), // %blah: blah
    Field(String, String),   // Field: bruh
    Blank,                   // Empty line
}

pub(crate) fn lex(s: String) -> Result<Vec<Token>, &'static str> {
    let mut tokens: Vec<Token> = Vec::new();

    let lines: Vec<String> = s.lines().map(|s| s.to_string()).collect();
    let mut it = lines.iter().peekable();

    while let Some(&line) = it.peek() {
        if line.trim().is_empty() {
            match tokens.last() {
                Some(Token::Blank) => (),
                _ => tokens.push(Token::Blank),
            };
            it.next();
            continue;
        }

        match line.chars().nth(0).unwrap() {
            '#' => {
                it.next();
            }
            '%' => {
                let (key, value) = read_line(&mut it)?;
                let key = key.trim_start_matches("%").to_string();
                tokens.push(Token::Keyword(key, value));
            }
            _ => {
                let (key, value) = read_line(&mut it)?;
                tokens.push(Token::Field(key, value));
            }
        }
    }

    Ok(tokens)
}

fn read_line<'a, I: Iterator<Item = &'a String>>(
    it: &mut Peekable<I>,
) -> Result<(String, String), &'static str> {
    let line = it.next().unwrap();

    match line.split_once(":") {
        None => Err("expected a colon"),
        Some((key, value)) if value.trim().is_empty() => {
            let mut text = String::from(value);

            while let Some(&line) = it.peek() {
                if !line.starts_with("+ ") {
                    break;
                }

                text.push_str(line.strip_prefix("+ ").unwrap());
                text.push('\n');
                it.next();
            }

            text.pop(); // Remove final newline

            Ok((key.to_string(), text))
        }
        Some((key, value)) => Ok((key.to_string(), value.trim().to_string())),
    }
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;

    #[test]
    fn tokenizer_simple() {
        assert_eq!(
            lex("%doc: My Books".to_owned()).unwrap(),
            vec![Keyword("doc".to_owned(), "My Books".to_owned())]
        );
    }

    #[test]
    fn tokenizer_multiline() {
        assert_eq!(
            lex("Field:\n+ Multi\n+ Line".to_owned()).unwrap(),
            vec![Field("Field".to_owned(), "Multi\nLine".to_owned())]
        );
    }

    #[test]
    fn tokenizer_blank() {
        assert_eq!(
            lex("Field: 1\n\n\n\n\nField: 2".to_owned()).unwrap(),
            vec![
                Field("Field".to_owned(), "1".to_owned()),
                Blank,
                Field("Field".to_owned(), "2".to_owned())
            ]
        );
    }
}
