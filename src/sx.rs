use super::{Err, Record};
use regex::Regex;
use std::convert::{TryFrom, TryInto};
use std::{cmp, iter};

// (1 + 1) > 1 ✔
// Age > 1
// ((Email ~ "foomail\.com") || (Age <= 18)) && !#Fixed

#[derive(Debug, PartialEq, Eq, Ord, Copy, Clone)]
enum Op {
    Mul,
    Div,
    Mod,
    Not,

    Add,
    Sub,

    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,

    //Before,
    //After,
    //Same,
    Concat,
    Match,

    And,
    Or,
}

impl Op {
    fn from(c: char, s: char) -> Self {
        use Op::*;

        match (c, s) {
            ('!', '=') => Ne,
            ('>', '=') => Ge,
            ('<', '=') => Le,
            ('&', '&') => And,
            ('|', '|') => Or,

            ('=', _) => Eq,
            ('>', _) => Gt,
            ('<', _) => Lt,
            ('&', _) => Concat,

            ('*', _) => Mul,
            ('/', _) => Div,
            ('%', _) => Mod,
            ('!', _) => Not,
            ('+', _) => Add,
            ('-', _) => Sub,
            ('~', _) => Match,

            _ => panic!("unknown operator: {}{}", c, s),
        }
    }
}

trait Operation<T: Sized> {
    fn op(op: Op, x: Self, x: Self) -> Self;
}

impl cmp::PartialOrd for Op {
    // operator precedence
    // taken from Go spec :p
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        use Op::*;

        fn ord(op: &Op) -> u8 {
            match op {
                Concat | Match => 6,
                Mul | Div | Mod | Not => 5,
                Add | Sub => 4,
                Eq | Ne | Gt | Ge | Lt | Le => 3,
                And => 2,
                Or => 1,
            }
        };

        Some(ord(self).cmp(&ord(other)))
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Int(isize),
    Str(String),
    Op(Op),
    Paren(bool), // true if opening
}

#[derive(Debug, PartialEq, Clone)]
struct Node {
    item: Token,
    children: Vec<Node>,
}

impl Node {
    /// create node without children
    fn new(t: Token) -> Self {
        Node {
            item: t,
            children: Vec::new(),
        }
    }

    fn eval(&self, rec: &Record) -> Result<Value, Err> {
        use self::Value::*;

        Ok(match &self.item {
            Token::Int(n) => Int(*n),
            Token::Str(s) => Str(s.clone()),
            Token::Op(op) => {
                let x = self
                    .children
                    .get(0)
                    .ok_or("expected at least one children")?
                    .eval(rec)?;

                match x {
                    Value::Int(x) => {
                        let y = self.children[1].eval(rec)?.try_into()?;
                        op_int(*op, x, y)
                    }
                    Value::Bool(x) => {
                        let y = match self.children.get(1) {
                            Some(node) => node.eval(rec)?.try_into()?,
                            None => false,
                        };
                        op_bool(*op, x, y)
                    }
                    Value::Str(x) => {
                        let y: String = self.children[1].eval(rec)?.try_into()?;
                        op_str(*op, &x, &y)
                    }
                    _ => todo!("real"),
                }
            }

            token => todo!("eval: {:?}", token),
        })
    }
}

// Who needs generics?
fn op_int(op: Op, x: isize, y: isize) -> Value {
    use Op::*;
    use Value::*;

    match op {
        Mul => Int(x * y),
        Div => Int(x / y),
        Add => Int(x + y),
        Sub => Int(x - y),
        Mod => Int(x % y),
        Eq => Bool(x == y),
        Ne => Bool(x != y),
        Gt => Bool(x > y),
        Ge => Bool(x >= y),
        Lt => Bool(x < y),
        Le => Bool(x <= y),
        _ => unreachable!("tried to perform {:?} on int value", op),
    }
}

fn op_real(op: Op, x: f64, y: f64) -> Value {
    use Op::*;
    use Value::*;

    match op {
        Mul => Real(x * y),
        Div => Real(x / y),
        Add => Real(x + y),
        Sub => Real(x - y),
        Mod => Real(x % y),
        Eq => Bool(x == y),
        Ne => Bool(x != y),
        Gt => Bool(x > y),
        Ge => Bool(x >= y),
        Lt => Bool(x < y),
        Le => Bool(x <= y),
        _ => unreachable!("tried to perform {:?} on real value", op),
    }
}

fn op_bool(op: Op, x: bool, y: bool) -> Value {
    use Op::*;
    use Value::*;

    match op {
        Or => Bool(x || y),
        And => Bool(x && y),
        Not => Bool(!x),
        _ => unreachable!("tried to perform {:?} on bool value", op),
    }
}

fn op_str(op: Op, x: &str, y: &str) -> Value {
    use Op::*;
    use Value::*;

    match op {
        Eq => Bool(x == y),
        Ne => Bool(x != y),
        Concat => Str([x, y].concat().to_owned()),
        Match => {
            // TODO compiled regex cache
            let rx = Regex::new(y).expect("compile regexp");
            Bool(rx.is_match(x))
        }
        _ => unreachable!("tried to perform {:?} on str value", op),
    }
}

#[derive(Debug)]
enum Value {
    Int(isize),
    Real(f64),
    Bool(bool),
    Str(String),
}

impl TryFrom<Value> for isize {
    type Error = Err;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(i) => Ok(i),
            Value::Str(s) => Ok(s.parse().map_err(|_| "failed to convert string to int")?),
            _ => Err("expected int")?,
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = Err;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Real(i) => Ok(i),
            _ => Err("expected f64")?,
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = Err;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Bool(b) => Ok(b),
            _ => Err("expected bool")?,
        }
    }
}

impl TryFrom<Value> for String {
    type Error = Err;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Str(s) => Ok(s.clone()),
            _ => Err("expected string")?,
        }
    }
}

#[derive(Debug)]
struct Sx(Node);

impl Sx {
    pub fn new(e: &str) -> Result<Self, Err> {
        Self::parse(lex(e)?)
    }

    fn parse(tokens: Vec<Token>) -> Result<Self, Err> {
        let mut nodes: Vec<Node> = tokens.iter().map(|t| Node::new(t.clone())).collect();

        // Fold expressions until there is only one node left
        while let Some(i) = find_first(&nodes) {
            fold_expr(&mut nodes, i)?;
        }
        assert!(nodes.len() == 1);

        let root = nodes.remove(0);

        if !matches!(root.item, Token::Op(_)) {
            Err("root node is not op")?
        }

        Ok(Sx(root))
    }

    pub fn eval(&self, rec: &Record) -> Result<bool, Err> {
        match self.0.eval(rec)? {
            Value::Bool(b) => Ok(b),
            Value::Int(n) => Ok(n != 0),
            _ => Ok(false),
        }
    }
}

fn fold_expr(nodes: &mut Vec<Node>, i: usize) -> Result<(), Err> {
    let mut i = i;
    let mut children = Vec::with_capacity(2);

    match nodes[i].item {
        // Not op only has one child
        Token::Op(Op::Not) => {
            children.push(nodes.remove(i + 1));
        }
        _ => {
            children.push(nodes.remove(i - 1));
            children.push(nodes.remove(i));
            i = i - 1;
        }
    }

    nodes[i].children = children;

    if i == 0 {
        return Ok(());
    }

    // remove parentheses around single expression
    let before = nodes.get(i - 1).map(|n| n.item.clone());
    let after = nodes.get(i + 1).map(|n| n.item.clone());

    match before.zip(after) {
        Some((Token::Paren(true), Token::Paren(false))) => {
            nodes.remove(i + 1);
            nodes.remove(i - 1);
        }
        _ => {}
    }

    Ok(())
}

/// returns index of first operation to execute
fn find_first(nodes: &[Node]) -> Option<usize> {
    let mut first_op = None;
    let mut idx = 0;

    let mut parens: isize = 0; // we are balancing parentheses here

    struct Paren {
        lvl: usize,
        idx: usize,
    };

    let mut max_paren = Paren { lvl: 0, idx: 0 };

    for (i, node) in nodes.iter().enumerate() {
        if node.children.len() > 0 {
            continue;
        }

        match node.item {
            Token::Op(op) if first_op.is_none() => {
                first_op = Some(op);
                idx = i;
            }
            Token::Op(op) if op > first_op.unwrap() => {
                first_op = Some(op);
                idx = i;
            }
            Token::Paren(opening) => {
                parens += if opening { 1 } else { -1 };

                if parens as usize > max_paren.lvl {
                    max_paren.lvl = parens as usize;
                    max_paren.idx = i;
                }
            }
            _ => continue,
        }
    }

    if parens != 0 {
        panic!("unmatched number of parens: {:?}", parens)
    }

    // parens override evaluation order
    if max_paren.lvl > 0 {
        let closing_pos = find_closing_paren(&nodes[max_paren.idx..]).unwrap();

        let start = max_paren.idx + 1;
        let end = start + closing_pos - 1;

        idx = start + find_first(&nodes[start..end]).unwrap();
    }

    first_op.map(|_| idx)
}

fn find_closing_paren(nodes: &[Node]) -> Option<usize> {
    let mut parens = 0;
    for (i, node) in nodes.iter().enumerate() {
        match node.item {
            Token::Paren(true) => parens += 1,
            Token::Paren(false) if parens - 1 == 0 => return Some(i),
            Token::Paren(false) => parens -= 1,
            _ => continue,
        }
    }
    None
}

fn lex(e: &str) -> Result<Vec<Token>, Err> {
    let mut tokens = Vec::new();
    let mut it = e.chars().peekable();

    while let Some(c) = it.peek() {
        match c {
            '*' | '/' | '%' | '!' | '+' | '-' | '=' | '>' | '<' | '&' | '|' | '~' => {
                let cur = it.next().unwrap();
                let next = it.peek().unwrap_or(&' ');

                let op = Op::from(cur, *next);

                // skip one more char if operator had 2 symbols
                if matches!(next, '=' | '&' | '|') {
                    it.next();
                }

                tokens.push(Token::Op(op));
            }

            '0'..='9' => {
                let n = parse_num(&mut it);
                tokens.push(Token::Int(n));
            }
            '(' | ')' => {
                tokens.push(Token::Paren(c == &'('));
                it.next();
            }
            '\'' | '"' => {
                let s = parse_string(&mut it);
                tokens.push(Token::Str(s))
            }
            ' ' => {
                it.next();
            }
            _ => todo!("lex: {}", c),
        };
    }

    Ok(tokens)
}

fn parse_num<I: Iterator<Item = char>>(it: &mut iter::Peekable<I>) -> isize {
    let mut n = 0;

    while let Some(c) = it.peek() {
        match c.to_digit(10) {
            Some(m) => n = n * 10 + m,
            None => break,
        }
        it.next();
    }

    n as isize
}

fn parse_string<I: Iterator<Item = char>>(it: &mut iter::Peekable<I>) -> String {
    let quote = it.next().unwrap();
    debug_assert!(quote == '\'' || quote == '"');

    let mut result = String::new();

    while let Some(c) = it.next() {
        match c {
            '\\' if it.peek().unwrap_or(&' ') == &quote => {
                // ignore escaped quotes
                result.push(it.next().unwrap());
            }
            _ if c == quote => {
                break;
            }
            c => result.push(c),
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn sexy() {
        // some basic stuff
        expect("33 > 22");
        expect("13 < '37'");
        expect("2 + 2 * 2 = 6");
        expect("(2 + 2) * 2 = 8");
        expect("(1 + 2) * (3 + 4) = 21");
        expect("(1 + 1 < 1) || (2 + 2 >= 4)");
        expect("(1 + 1 <= 2) && (2 + 2 > 2)");
        expect("!(2 = 1)");
        expect("!(1 = 1 && 1 = 2)");

        // strings
        expect("'foo' = 'foo'");
        expect("'bar' != 'baz'");
        expect("'foo' & 'bar' = 'foobar'");
        expect("'bar' ~ 'b.r'");
        expect("!('bus' ~ 'b.r')");
    }

    fn expect(s: &str) {
        let sx = Sx::new(s).expect("parse");
        let empty = HashMap::new(); // Can't read fields yet

        if !sx.eval(&empty).expect("eval") {
            panic!("{} evaluated to false. \nast: {:#?}", s, sx.0)
        };
    }

    #[test]
    fn parse_str() {
        let mut it = "'hello \\'mom'".chars().peekable();
        assert_eq!("hello 'mom", parse_string(&mut it));

        let mut it = "\"this also\nworks\"".chars().peekable();
        assert_eq!("this also\nworks", parse_string(&mut it));
    }
}
