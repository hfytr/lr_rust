use std::fmt::Display;

#[derive(Debug, Clone)]
enum Node {
    Expr(Vec<Box<Node>>),
    Term(Vec<Box<Node>>),
    Literal(usize),
    Multiply,
    Plus,
    LeftParen,
    RightParen,
}

impl Node {
    fn eval(&self) -> usize {
        match self {
            Node::Expr(terms) => terms.into_iter().map(|node| node.eval()).sum(),
            Node::Term(factors) => factors.into_iter().map(|node| node.eval()).product(),
            Node::Literal(val) => *val,
            _ => panic!("Cannot evaluate lexeme."),
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Expr(terms) => {
                f.write_str("(")?;
                for (i, term) in terms.iter().enumerate() {
                    term.fmt(f)?;
                    if i != terms.len() - 1 {
                        f.write_str(" + ")?;
                    }
                }
                f.write_str(")")?;
            }
            Node::Term(factors) => {
                for (i, factor) in factors.iter().enumerate() {
                    factor.fmt(f)?;
                    if i != factors.len() - 1 {
                        f.write_str(" * ")?;
                    }
                }
            }
            Node::Literal(val) => write!(f, "{val}")?,
            _ => panic!("Cannot display lexeme."),
        }
        Ok(())
    }
}

fn expr_node(term: Node, mut expr: Node) -> Node {
    if let Node::Expr(ref mut terms) = expr {
        terms.push(Box::new(term));
    }
    expr
}

fn term_node(factor: Node, mut term: Node) -> Node {
    if let Node::Term(ref mut factors) = term {
        factors.push(Box::new(factor));
    }
    term
}

parser::parser! {
    State(()),
    Output(Node),
    Kind(NodeKind),
    GeneratedFn(create_parsing_engine),
    Expr => Rule(
        Term Plus Expr |_, term, _, expr| expr_node(term, expr),
        Term |_, term| Node::Expr(vec![Box::new(term)])
    ),
    Term => Rule(
        Factor Multiply Term |_, factor, _, term| term_node(factor, term),
        Factor |_, factor| Node::Term(vec![Box::new(factor)])
    ),
    Factor => Rule(
        Literal,
        LeftParen Expr RightParen |_, _, expr, _| expr
    ),
    _ => Regex(" *" |_, _| None),
    Literal => Regex("[0-9]*" |_, text: &str| {
        Some((Node::Literal(text.parse().unwrap()), NodeKind::Literal as usize))
    }),
    Multiply => Literal("*" |_, _| Some((Node::Multiply, NodeKind::Multiply as usize))),
    Multiply => Literal("x" |_, _| Some((Node::Multiply, NodeKind::Multiply as usize))),
    Plus => Literal("+" |_, _| Some((Node::Plus, NodeKind::Plus as usize))),
    LeftParen => Literal("(" |_, _| Some((Node::LeftParen, NodeKind::LeftParen as usize))),
    RightParen => Literal(")" |_, _| Some((Node::RightParen, NodeKind::RightParen as usize))),
}

#[test]
fn parse_expression_language() {
    let s = String::from("1*7*(5+7)+3*(5+7*(6+9))x(6)");
    let mut engine = create_parsing_engine().unwrap();
    let mut state = ();
    let expr = engine
        .parse(NodeKind::Expr as usize, &s, &mut state)
        .unwrap();
    dbg!(&expr);
    dbg!(expr.eval());
    assert_eq!(expr.eval(), 2064);
}

#[test]
fn parse_term() {
    let s = String::from("1  *7*(5+7  )");
    let mut engine = create_parsing_engine().unwrap();
    let mut state = ();
    let expr = engine
        .parse(NodeKind::Term as usize, &s, &mut state)
        .unwrap();
    dbg!(&expr);
    dbg!(expr.eval());
    assert_eq!(expr.eval(), 84);
}

#[test]
fn parse_literal() {
    let s = String::from("555");
    let mut engine = create_parsing_engine().unwrap();
    let mut state = ();
    let expr = engine
        .parse(NodeKind::Factor as usize, &s, &mut state)
        .unwrap();
    println!("{}", expr);
    dbg!(expr.eval());
    assert_eq!(expr.eval(), 555);
}
