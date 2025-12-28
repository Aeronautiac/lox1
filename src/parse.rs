use super::lex::{MetaToken, Token};
use std::{collections::HashMap, mem::Discriminant, mem::discriminant, sync::LazyLock};

enum Direction {
    Left,
    Right,
}

// higher precedence value -> higher eval priority
type PrecedenceValue = u8;
struct OperatorData {
    assoc_or_fixity: Direction,
    precedence: PrecedenceValue,
}

impl OperatorData {
    fn new(precedence: PrecedenceValue, assoc_or_fixity: Direction) -> Self {
        Self {
            precedence,
            assoc_or_fixity, // for unary fixity, left means prefix, right means postfix
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
        OperatorData::new(5, Direction::Left),
    );
    map.insert(
        Operator::new(Token::Not, OperatorType::Unary),
        OperatorData::new(5, Direction::Left),
    );

    // BINARY ARITHMETIC
    map.insert(
        Operator::new(Token::Add, OperatorType::Binary),
        OperatorData::new(10, Direction::Left),
    );
    map.insert(
        Operator::new(Token::Subtract, OperatorType::Binary),
        OperatorData::new(10, Direction::Left),
    );
    map.insert(
        Operator::new(Token::Multiply, OperatorType::Binary),
        OperatorData::new(15, Direction::Left),
    );
    map.insert(
        Operator::new(Token::Divide, OperatorType::Binary),
        OperatorData::new(15, Direction::Left),
    );

    // BINARY LOGICAL
    map.insert(
        Operator::new(Token::And, OperatorType::Binary),
        OperatorData::new(3, Direction::Left),
    );
    map.insert(
        Operator::new(Token::Or, OperatorType::Binary),
        OperatorData::new(3, Direction::Left),
    );
    map.insert(
        Operator::new(Token::GreaterThan, OperatorType::Binary),
        OperatorData::new(3, Direction::Left),
    );
    map.insert(
        Operator::new(Token::GreaterThanOrEqual, OperatorType::Binary),
        OperatorData::new(3, Direction::Left),
    );
    map.insert(
        Operator::new(Token::LessThan, OperatorType::Binary),
        OperatorData::new(3, Direction::Left),
    );
    map.insert(
        Operator::new(Token::LessThanOrEqual, OperatorType::Binary),
        OperatorData::new(3, Direction::Left),
    );
    map.insert(
        Operator::new(Token::EqualTo, OperatorType::Binary),
        OperatorData::new(3, Direction::Left),
    );
    map.insert(
        Operator::new(Token::NotEqualTo, OperatorType::Binary),
        OperatorData::new(3, Direction::Left),
    );

    // BINARY MISC
    map.insert(
        Operator::new(Token::Assignment, OperatorType::Binary),
        OperatorData::new(1, Direction::Left),
    );

    map
});

enum Expression {
    Binary {
        operator: Operator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Unary {
        operator: Operator,
        target: Box<Expression>,
    },
    Value(MetaToken),
}

enum Statement {}

pub struct Parser {
    tokens: Vec<MetaToken>,
    remaining_lookahead: usize,
    curr_tok_index: usize,
}

impl Parser {
    pub fn feed(&mut self, tok: MetaToken) {
        self.tokens.push(tok);
        self.advance();
    }

    fn set_look_ahead(&mut self, amt: usize) {
        assert!(
            !self.is_looking_ahead(),
            "Attempt to look ahead while in lookahead state"
        );
        self.remaining_lookahead = amt;
    }

    fn is_looking_ahead(&mut self) -> bool {
        self.remaining_lookahead > 0
    }

    fn look_ahead(&mut self, amt: usize) -> &MetaToken {
        &self.tokens[self.curr_tok_index + amt]
    }

    fn look_back(&mut self, amt: usize) -> &MetaToken {
        &self.tokens[self.curr_tok_index - amt]
    }

    fn advance(&mut self) {
        if self.is_looking_ahead() {
            self.remaining_lookahead -= 1;
            return;
        }
    }

    fn peek(&self) -> &MetaToken {
       &self.tokens[self.curr_tok_index] 
    }
}
