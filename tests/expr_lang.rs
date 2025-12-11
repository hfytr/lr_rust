use std::fmt::Display;

impl Node {
    fn eval(&self) -> usize {
        match self {
            Node::Expr { terms } => terms.into_iter().map(|node| node.eval()).sum(),
            Node::Term { factors } => factors.into_iter().map(|node| node.eval()).product(),
            Node::Literal(val) => *val,
            _ => panic!("Cannot evaluate lexeme."),
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Expr { terms } => {
                f.write_str("(")?;
                for (i, term) in terms.iter().enumerate() {
                    term.fmt(f)?;
                    if i != terms.len() - 1 {
                        f.write_str(" + ")?;
                    }
                }
                f.write_str(")")?;
            }
            Node::Term { factors } => {
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
    if let Node::Expr { ref mut terms } = expr {
        terms.push(Box::new(term));
    }
    expr
}

fn term_node(factor: Node, mut term: Node) -> Node {
    if let Node::Term { ref mut factors } = term {
        factors.push(Box::new(factor));
    }
    term
}

fn merge_span(_a: &mut (), _b: &()) {
    ()
}

lr_rust::parser! {
    State(()),
    Output(Node),
    Kind(NodeKind),
    Enum(Node),
    GeneratedFn(create_parsing_engine),
    Span((), (), merge_span),
    Expr { terms: Vec<Box<Node>> } -> Rule(
        Term Plus Expr |_, _, term, _, expr| expr_node(term, expr),
        Term |_, _, term| Node::Expr{terms:vec![Box::new(term)]}
    ),
    Term { factors: Vec<Box<Node>> } -> Rule(
        Factor Multiply Term |_, _, factor, _, term| term_node(factor, term),
        Factor |_, _, factor| Node::Term{factors:vec![Box::new(factor)]}
    ),
    Factor -> Rule(
        Literal,
        LeftParen Expr RightParen |_, _, _, expr, _| expr
    ),
    _ -> Regex(" *" |_, _| None),
    Literal(usize) -> Regex("[0-9]*" |_, text: &str| {
        Some((Node::Literal(text.parse().unwrap()), (), NodeKind::Literal as usize))
    }),
    Multiply -> Literal("*" |_, _| Some((Node::Multiply, (), NodeKind::Multiply as usize))),
    Multiply -> Literal("x" |_, _| Some((Node::Multiply, (), NodeKind::Multiply as usize))),
    Plus -> Literal("+" |_, _| Some((Node::Plus, (), NodeKind::Plus as usize))),
    LeftParen -> Literal("(" |_, _| Some((Node::LeftParen, (), NodeKind::LeftParen as usize))),
    RightParen -> Literal(")" |_, _| Some((Node::RightParen, (), NodeKind::RightParen as usize))),
}

#[test]
fn parse_expression_language() {
    let s = String::from("1*7*(5+7)+3*(5+7*(6+9))x(6)");
    let mut engine = create_parsing_engine().unwrap();
    let mut state = ();
    let expr = engine.parse(NodeKind::Expr as usize, &s, &mut state).unwrap();
    assert_eq!(expr.eval(), 2064);
}

#[test]
fn parse_term() {
    let s = String::from("1  *7*(5+7  )");
    let mut engine = create_parsing_engine().unwrap();
    let mut state = ();
    let expr = engine.parse(NodeKind::Term as usize, &s, &mut state).unwrap();
    assert_eq!(expr.eval(), 84);
}

#[test]
fn parse_literal() {
    let s = String::from("555");
    let mut engine = create_parsing_engine().unwrap();
    let mut state = ();
    let expr = engine.parse(NodeKind::Factor as usize, &s, &mut state).unwrap();
    assert_eq!(expr.eval(), 555);
}
