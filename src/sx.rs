use super::Err;
use std::cmp; // ((Email ~ "foomail\.com") || (Age <= 18)) && !#Fixed
              // Age > 1
              // (1 + 1) > 1

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

    //Concat,
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

            ('*', _) => Mul,
            ('/', _) => Div,
            ('%', _) => Mod,
            ('!', _) => Not,
            ('+', _) => Add,
            ('-', _) => Sub,

            _ => panic!("unknown operator: {}{}", c, s),
        }
    }
}

impl cmp::PartialOrd for Op {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        use Op::*;

        // operator precedence
        fn ord(op: &Op) -> u8 {
            match op {
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

#[derive(Debug, PartialEq, Copy, Clone)]
enum Token {
    Int(isize),
    Op(Op),
    Paren(bool), // true if closing
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
}

#[derive(Debug)]
struct Sx(Node);

impl Sx {
    fn new(e: &str) -> Result<Self, Err> {
        Self::parse(lex(e)?)
    }

    fn parse(tokens: Vec<Token>) -> Result<Self, Err> {
        let mut nodes: Vec<Node> = tokens.iter().map(|t| Node::new(*t)).collect();

        // Fold expressions until there is only one node left
        while let Some(i) = find_first(&mut nodes) {
            println!("{}: {:#?}", i, nodes);
            fold_expr(&mut nodes, i)?;
        }
        assert!(nodes.len() == 1);

        Ok(Sx(nodes.remove(0)))
    }
}

fn fold_expr(nodes: &mut Vec<Node>, i: usize) -> Result<(), Err> {
    let token = nodes[i].item;

    let next = nodes.remove(i + 1);
    assert_eq!(matches!(next.item, Token::Paren(_)), false);

    let prev = nodes.remove(i - 1);
    assert_eq!(matches!(prev.item, Token::Paren(_)), false);

    nodes[i - 1] = Node {
        item: token,
        children: vec![prev, next],
    };

    Ok(())
}

/// returns index of first operation to execute
fn find_first(tokens: &mut Vec<Node>) -> Option<usize> {
    let mut first_op = None;
    let mut idx = 0;

    let mut parens = 0; // we are balancing parentheses here
    let mut max_paren = (0, 0);

    for (i, node) in tokens.iter().enumerate() {
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
            Token::Paren(closing) => {
                if closing {
                    parens -= 1
                } else {
                    parens += 1
                };

                if parens > max_paren.0 {
                    max_paren = (parens, i);
                }
            }
            _ => continue,
        }
    }

    if parens != 0 {
        panic!("unmatched number of parens: {:?}", parens)
    }

    if max_paren.0 > 0 {
        tokens.remove(max_paren.1);
        tokens.remove(max_paren.1 + 3);

        idx = max_paren.1 + 1;
    }

    first_op.map(|_| idx)
}

fn lex(e: &str) -> Result<Vec<Token>, Err> {
    let mut tokens = Vec::new();
    let mut it = e.chars().peekable();

    while let Some(c) = it.peek() {
        match c {
            '*' | '/' | '%' | '!' | '+' | '-' | '=' | '>' | '<' | '&' | '|' => {
                let op = Op::from(it.next().unwrap(), *it.peek().unwrap_or(&' '));
                tokens.push(Token::Op(op));
            }

            '0'..='9' => {
                let n = parse_num(&mut it);
                tokens.push(Token::Int(n));
            }
            '(' => {
                tokens.push(Token::Paren(false));
                it.next();
            }
            ')' => {
                tokens.push(Token::Paren(true));
                it.next();
            }
            ' ' => {
                it.next();
            }
            _ => todo!("lex: {}", c),
        };
    }

    Ok(tokens)
}

fn parse_num<I: Iterator<Item = char>>(it: &mut std::iter::Peekable<I>) -> isize {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sexy() {
        assert_eq!(
            Sx::new("33 > 22").unwrap().0,
            Node {
                item: Token::Op(Op::Gt),
                children: vec![
                    Node {
                        item: Token::Int(33),
                        children: Vec::new(),
                    },
                    Node {
                        item: Token::Int(22),
                        children: Vec::new(),
                    }
                ]
            }
        )
    }

    #[test]
    fn sexy_2() {
        assert_eq!(
            Sx::new("2 + 2 * 2").unwrap().0,
            Node {
                item: Token::Op(Op::Add),
                children: vec![
                    Node::new(Token::Int(2)),
                    Node {
                        item: Token::Op(Op::Mul),
                        children: vec![Node::new(Token::Int(2)), Node::new(Token::Int(2)),],
                    }
                ]
            }
        )
    }

    #[test]
    fn sexy_3() {
        assert_eq!(
            Sx::new("(2 + 2) * 2").unwrap().0,
            Node {
                item: Token::Op(Op::Mul),
                children: vec![
                    Node {
                        item: Token::Op(Op::Add),
                        children: vec![Node::new(Token::Int(2)), Node::new(Token::Int(2)),],
                    },
                    Node::new(Token::Int(2)),
                ]
            }
        )
    }
    #[test]
    fn sexy_4() {
        println!("{:#?}", Sx::new("(1 + 2) + (3 + 4)").unwrap());
        println!("{:#?}", Sx::new("1 + (2 + (3 + 4))").unwrap());
        //println!("{:#?}", Sx::new("1 + (2 + (3 + 4 + 5))").unwrap());
        todo!("assertions");
    }
}
