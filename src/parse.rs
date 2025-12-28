use super::lex::Token;
use std::collections::HashMap;
use std::mem::discriminant;
use std::{mem::Discriminant, sync::LazyLock};

enum Expression {}

enum Statement {}

enum Associativity {
    Left,
    Right,
}

// higher precedence value -> higher eval priority
type PrecedenceValue = u8;
struct OperatorData {
    associativity: Associativity,
    precedence: PrecedenceValue,
}

impl OperatorData {
    fn new(precedence: PrecedenceValue, associativity: Associativity) -> Self {
        Self {
            precedence,
            associativity, // for unary operators, this determines the side in which it is applied. left means prefix, right means postfix
        }
    }
}

#[derive(Hash, PartialEq, Eq)]
enum OperatorType {
    Unary,
    Binary,
}

#[derive(Hash, PartialEq, Eq)]
struct Operator {
    tok: Discriminant<Token>,
    op_type: OperatorType,
}

impl Operator {
    fn new(tok: Token, op_type: OperatorType) -> Self {
        Self {
            tok: discriminant(&tok),
            op_type,
        }
    }
}

static OPERATORS: LazyLock<HashMap<Operator, OperatorData>> = LazyLock::new(|| {
    let mut map: HashMap<Operator, OperatorData> = HashMap::new();

    // UNARY
    map.insert(
        Operator::new(Token::Subtract, OperatorType::Unary),
        OperatorData::new(5, Associativity::Left),
    );
    map.insert(
        Operator::new(Token::Not, OperatorType::Unary),
        OperatorData::new(5, Associativity::Left),
    );

    // BINARY ARITHMETIC
    map.insert(
        Operator::new(Token::Add, OperatorType::Binary),
        OperatorData::new(10, Associativity::Left),
    );
    map.insert(
        Operator::new(Token::Subtract, OperatorType::Binary),
        OperatorData::new(10, Associativity::Left),
    );
    map.insert(
        Operator::new(Token::Multiply, OperatorType::Binary),
        OperatorData::new(15, Associativity::Left),
    );
    map.insert(
        Operator::new(Token::Divide, OperatorType::Binary),
        OperatorData::new(15, Associativity::Left),
    );

    // BINARY LOGICAL
    map.insert(
        Operator::new(Token::And, OperatorType::Binary),
        OperatorData::new(3, Associativity::Left),
    ); 
    map.insert(
        Operator::new(Token::Or, OperatorType::Binary),
        OperatorData::new(3, Associativity::Left),
    );
    map.insert(
        Operator::new(Token::GreaterThan, OperatorType::Binary),
        OperatorData::new(3, Associativity::Left),
    );
    map.insert(
        Operator::new(Token::GreaterThanOrEqual, OperatorType::Binary),
        OperatorData::new(3, Associativity::Left),
    );
    map.insert(
        Operator::new(Token::LessThan, OperatorType::Binary),
        OperatorData::new(3, Associativity::Left),
    );
    map.insert(
        Operator::new(Token::LessThanOrEqual, OperatorType::Binary),
        OperatorData::new(3, Associativity::Left),
    );
    map.insert(
        Operator::new(Token::EqualTo, OperatorType::Binary),
        OperatorData::new(3, Associativity::Left),
    );
    map.insert(
        Operator::new(Token::NotEqualTo, OperatorType::Binary),
        OperatorData::new(3, Associativity::Left),
    );

    // BINARY MISC
    map.insert(
        Operator::new(Token::Assignment, OperatorType::Binary),
        OperatorData::new(1, Associativity::Left),
    );

    map
});

pub struct Parser {}
