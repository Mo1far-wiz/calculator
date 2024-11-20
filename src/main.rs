use pest::Parser;
use pest::pratt_parser::{Assoc::*, Op, PrattParser};
use pest_derive::Parser;
use std::io::{self, BufRead};

#[derive(Parser)]
#[grammar = "grammar.pest"] // Pointing to the grammar file
struct CalculatorParser;

#[derive(Debug)]
pub enum Expr {
    Integer(i32),
    UnaryMinus(Box<Expr>),
    BinOp {
        lhs: Box<Expr>,
        op: OpKind,
        rhs: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum OpKind {
    Add,
    Subtract,
    Multiply,
    Divide,
}

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        PrattParser::new()
            .op(Op::infix(Rule::add, Left) | Op::infix(Rule::subtract, Left))
            .op(Op::infix(Rule::multiply, Left) | Op::infix(Rule::divide, Left))
            .op(Op::prefix(Rule::unary_minus))
    };
}

impl Expr {
    pub fn evaluate(&self) -> i32 {
        match self {
            Expr::Integer(value) => *value,
            Expr::UnaryMinus(expr) => -expr.evaluate(),
            Expr::BinOp { lhs, op, rhs } => {
                let left = lhs.evaluate();
                let right = rhs.evaluate();
                match op {
                    OpKind::Add => left + right,
                    OpKind::Subtract => left - right,
                    OpKind::Multiply => left * right,
                    OpKind::Divide => left / right,
                }
            }
        }
    }
}

fn parse_expr(pairs: pest::iterators::Pairs<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::integer => Expr::Integer(primary.as_str().parse::<i32>().unwrap()),
            Rule::expr => parse_expr(primary.into_inner()),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::unary_minus => Expr::UnaryMinus(Box::new(rhs)),
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::add => OpKind::Add,
                Rule::subtract => OpKind::Subtract,
                Rule::multiply => OpKind::Multiply,
                Rule::divide => OpKind::Divide,
                _ => unreachable!(),
            };
            Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        })
        .parse(pairs)
}

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let input = stdin.lock();

    for line in input.lines() {
        let line = line?;
        match CalculatorParser::parse(Rule::equation, &line) {
            Ok(mut pairs) => {
                let expr = parse_expr(pairs.next().unwrap().into_inner());
                println!("Result: {}", expr.evaluate());
            }
            Err(e) => eprintln!("Parse failed: {:?}", e),
        }
    }

    Ok(())
}
