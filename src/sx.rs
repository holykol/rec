use super::Err;
use std::cmp;

// (1 + 1) > 1
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
        while let Some(i) = find_first(&nodes) {
            fold_expr(&mut nodes, i)?;
        }
        assert!(nodes.len() == 1);

        Ok(Sx(nodes.remove(0)))
    }
}

fn fold_expr(nodes: &mut Vec<Node>, i: usize) -> Result<(), Err> {
    let token = nodes[i].item;

    let next = nodes.remove(i + 1);
    assert!(!matches!(next.item, Token::Paren(_)), "next is paren");

    let prev = nodes.remove(i - 1);
    assert!(!matches!(prev.item, Token::Paren(_)), "prev is paren");

    // elements have shifted by one
    let i = i - 1;

    nodes[i] = Node {
        item: token,
        children: vec![prev, next],
    };

    if i == 0 {
        return Ok(());
    }

    // remove parentheses around single expression
    let before = nodes.get(i - 1).map(|n| n.item);
    let after = nodes.get(i + 1).map(|n| n.item);

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
    println!("finding first for {:#?}\n\n", nodes);
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
    println!("finding closing parens for: {:#?}", nodes);
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
            '*' | '/' | '%' | '!' | '+' | '-' | '=' | '>' | '<' | '&' | '|' => {
                let op = Op::from(it.next().unwrap(), *it.peek().unwrap_or(&' '));
                tokens.push(Token::Op(op));
            }

            '0'..='9' => {
                let n = parse_num(&mut it);
                tokens.push(Token::Int(n));
            }
            '(' => {
                tokens.push(Token::Paren(true));
                it.next();
            }
            ')' => {
                tokens.push(Token::Paren(false));
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
    fn sexy_basic() {
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
    fn sexy_precedence() {
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
    fn sexy_braces() {
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
    fn sexy_complex() {
        println!("{:#?}", Sx::new("(1 + 2) + (3 + 4)").unwrap());
        println!("{:#?}", Sx::new("1 + (2 + (3 + 4))").unwrap());
        println!("{:#?}", Sx::new("1 + (2 + (3 + 4 + 5))").unwrap());
        //todo!("assertions");
    }
}
