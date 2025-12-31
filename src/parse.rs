use super::lex::{MetaToken, Token};
use std::{collections::HashMap, mem::Discriminant, mem::discriminant, sync::LazyLock};

enum Direction {
    Left,
    Right,
}

// 0 is left, 1 is right
// to handle associativity:
// treat left bindng power as "base" associativity
// for left associativty, right bp is equal to left bp
// for right associtiativity, right bp is equal to left bp - 1
// due to this convention, the gaps in binding powers between precedence levels should be at least 2.
// for future convenience, an increment of 5 between base values in precedence levels will be chosen.
type BpVal = u8;
type BindingPower = (BpVal, BpVal);
type Operator = Discriminant<Token>;

fn l_assoc(bp: BpVal) -> BindingPower {
    (bp, bp)
}

fn r_assoc(bp: BpVal) -> BindingPower {
    (bp, bp - 1)
}

fn op(tok: &Token) -> Operator {
    discriminant(tok)
}

static PREFIX_OPS: LazyLock<HashMap<Operator, BindingPower>> = LazyLock::new(|| {
    let mut map: HashMap<Operator, BindingPower> = HashMap::new();
    let prefix = 25;

    // ARITHMETIC
    map.insert(op(&Token::Subtract), (0, prefix));

    // LOGICAL
    map.insert(op(&Token::Not), (0, prefix));

    map
});

static POSTFIX_OPS: LazyLock<HashMap<Operator, BindingPower>> = LazyLock::new(|| {
    let mut map: HashMap<Operator, BindingPower> = HashMap::new();

    // ARITHMETIC

    map
});

static INFIX_OPS: LazyLock<HashMap<Operator, BindingPower>> = LazyLock::new(|| {
    let mut map: HashMap<Operator, BindingPower> = HashMap::new();

    // ARITHMETIC
    let arith_lo = 15;
    let arith_hi = 20;
    map.insert(op(&Token::Add), l_assoc(arith_lo));
    map.insert(op(&Token::Subtract), l_assoc(arith_lo));
    map.insert(op(&Token::Multiply), l_assoc(arith_hi));
    map.insert(op(&Token::Divide), l_assoc(arith_hi));

    // LOGICAL
    let logical = 10;
    map.insert(op(&Token::And), l_assoc(logical));
    map.insert(op(&Token::Or), l_assoc(logical));
    map.insert(op(&Token::GreaterThan), l_assoc(logical));
    map.insert(op(&Token::GreaterThanOrEqual), l_assoc(logical));
    map.insert(op(&Token::LessThan), l_assoc(logical));
    map.insert(op(&Token::LessThanOrEqual), l_assoc(logical));
    map.insert(op(&Token::EqualTo), l_assoc(logical));
    map.insert(op(&Token::NotEqualTo), l_assoc(logical));

    // BINARY MISC 
    map.insert(op(&Token::Dot), l_assoc(30));
    map.insert(op(&Token::Assignment), r_assoc(5));

    map
});

fn prefix_op(tok: &Token) -> Option<&BindingPower> {
    PREFIX_OPS.get(&op(tok))
}

fn infix_op(tok: &Token) -> Option<&BindingPower> {
    INFIX_OPS.get(&op(tok))
}

fn postfix_op(tok: &Token) -> Option<&BindingPower> {
    POSTFIX_OPS.get(&op(tok))
}

pub enum Expression {
    Binary {
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Unary {
        operator: Token,
        target: Box<Expression>,
    },
    Value(Token),
}

pub enum Statement {}

pub struct Parser {
    tokens: Vec<MetaToken>,
    curr_tok_index: usize,
    min_bp: BpVal,
    next_token: dyn FnMut() -> MetaToken,
}

pub enum ParseError {
    UnknownExpression,
}

type ExprMatchResult = Result<Box<Expression>, ParseError>;

impl Parser {
    pub fn parse(&mut self) -> Vec<Box<Statement>> {
        let stmts: Vec<Box<Statement>> = vec![];
        loop {
            let meta = (self.next_token)();
            let tok = meta.tok;
            if tok == Token::EOF {
                break;
            }

            match tok {
                _ => {}
            }
        }
        stmts
    }

    fn peek_next(&mut self) -> &MetaToken {
        self.tokens.push((self.next_token)());
        &self.tokens[self.curr_tok_index + 1]
    }

    fn peek_last(&mut self) -> Option<&MetaToken> {
        if self.curr_tok_index == 0 {
            None
        } else {
            Some(&self.tokens[self.curr_tok_index - 1])
        }
    }

    fn advance(&mut self) -> &MetaToken {
        if self.curr_tok_index >= self.tokens.len() - 1 {
            self.tokens.push((self.next_token)());
        }
        self.curr_tok_index += 1;
        self.peek()
    }

    fn peek(&self) -> &MetaToken {
        &self.tokens[self.curr_tok_index]
    }

    // null denotation handler
    // attempts to match a nud expr
    // returns the expression node if matched
    // otherwise, returns a ParseError
    // prefix is handled in led
    fn nud_expr(&mut self) -> ExprMatchResult {
        let curr_tok_meta = self.advance().clone();
        let curr_tok = &curr_tok_meta.tok;

        match curr_tok {
            Token::Number(_) | Token::String(_) | Token::Boolean(_) | Token::Identifier(_) => {
                self.value_expr()
            }
            Token::OpeningParen => self.paren(),
            _ if self.is_prefix_op() => self.prefix_op(),
            _ => {
                unimplemented!()
            }
        }
    }

    // call expr and then ensure that it ends with a (
    // temporarily just call expr
    fn paren(&mut self) -> ExprMatchResult {
        self.expr()
    }

    fn is_prefix_op(&mut self) -> bool {
        prefix_op(&self.peek().tok).is_some()
    }

    fn value_expr(&mut self) -> ExprMatchResult {
        let curr_tok: &MetaToken = self.peek();
        Ok(Box::new(Expression::Value(curr_tok.tok.clone())))
    }

    fn prefix_op(&mut self) -> ExprMatchResult {
        let curr = self.peek();
        let rbp = prefix_op(&curr.tok).unwrap().1;
        Ok(Box::new(Expression::Unary {
            operator: curr.tok.clone(),
            target: self.expr_bp(rbp)?,
        }))
    }

    fn led_expr(&mut self) -> ExprMatchResult {}

    fn postfix_op(&mut self, child: Expression) -> ExprMatchResult {}
    // first, we parse some initial expression (nud) (value, unary op, misc expr (if, block, match, etc...) or paren expr).
    // that first expression is saved as node.
    // then, while there is a binary operator (led) and the precedence/binding power of the operator being parsed is higher than the passed binding power,
    // recursively build the left subtree using this node.
    // (when building a binary expression, you pass in a left expression. the binary expression handler returns a binary expression node with a right expression
    // which is parsed in handle. node is replaced by this new subtree.)
    // when you are done, you return node
    // in top level expression calls, pass a binding power of 0
    // in some cases, binding power needs to be overwritten. most of the time, it is propagated. this means that most expression handlers
    // require a binding power parameter to propagate back to parse_expr. either this, or the parser struct holds a binding power value (this sounds nicer)
    fn expr(&mut self) -> ExprMatchResult {
        self.expr_bp(0)
    }

    fn expr_bp(&mut self, bp: BpVal) -> ExprMatchResult {
        self.min_bp = bp;
        let mut left = self.nud_expr();
        loop {}
    }
}
