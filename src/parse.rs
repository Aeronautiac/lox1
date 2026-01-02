use super::lex::{MetaToken, Token};
use std::{
    collections::{HashMap, HashSet},
    mem::Discriminant,
    mem::discriminant,
    sync::LazyLock,
};

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

// stores any nud tokens which may not appear in the prefix operator map
static EXTRA_NUD_TOKENS: LazyLock<HashSet<Discriminant<Token>>> = LazyLock::new(|| {
    let mut set: HashSet<Discriminant<Token>> = HashSet::new();

    set.insert(discriminant(&Token::String(vec![])));
    set.insert(discriminant(&Token::Number(0.0)));
    set.insert(discriminant(&Token::Identifier(vec![])));
    set.insert(discriminant(&Token::Nil));
    set.insert(discriminant(&Token::Boolean(true)));
    set.insert(discriminant(&Token::OpeningParen));
    set.insert(discriminant(&Token::This));

    set
});

// return true if the token may be parsed as part of a nud expression
fn is_nud(tok: &Token) -> bool {
    prefix_op(tok).is_some() || EXTRA_NUD_TOKENS.contains(&discriminant(tok))
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

pub enum Statement {
    Block { statements: Vec<Statement> },
}

pub struct Parser {
    tokens: Vec<MetaToken>,
    curr_tok_index: usize,
    min_bp: BpVal,
    next_token: dyn FnMut() -> MetaToken,
}

pub enum ParseError {
    UnknownExpression,
    AmbiguousExpression,
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

    fn nud_expr(&mut self) -> ExprMatchResult {
        let curr_tok_meta = self.advance().clone();
        let curr_tok = &curr_tok_meta.tok;

        match curr_tok {
            Token::Number(_)
            | Token::String(_)
            | Token::Boolean(_)
            | Token::Identifier(_)
            | Token::This
            | Token::Nil => self.value_expr(),
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

    fn is_postfix_op(&mut self) -> bool {
        postfix_op(&self.peek().tok).is_some() && !self.is_infix_op()
    }

    // look ahead by one token to determine if the next token
    // is NUD
    fn is_infix_op(&mut self) -> bool {
        infix_op(&self.peek().tok).is_some() && is_nud(&self.peek_next().tok)
    }

    // ambiguous when:
    // current token can either be infix or postfix
    // and
    // next token can be infix and (prefix or postfix)
    fn is_ambiguous_led(&mut self) -> bool {
        let next_tok = self.peek_next().tok.clone();
        let curr_tok = &self.peek().tok;
        (infix_op(curr_tok).is_some() && postfix_op(curr_tok).is_some())
            && (infix_op(&next_tok).is_some()
                && (prefix_op(&next_tok).is_some() || postfix_op(&next_tok).is_some()))
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

    fn infix_op(&mut self, lhs: Box<Expression>) -> ExprMatchResult {
        let curr = &self.peek().tok;
        let rbp = infix_op(curr).unwrap().1;
        Ok(Box::new(Expression::Binary {
            operator: curr.clone(),
            left: lhs,
            right: self.expr_bp(rbp)?,
        }))
    }

    fn postfix_op(&mut self, child: Box<Expression>) -> ExprMatchResult {
        let curr = &self.peek().tok;
        let rbp = postfix_op(curr).unwrap().1;
        Ok(Box::new(Expression::Unary {
            operator: curr.clone(),
            target: child,
        }))
    }

    fn expr(&mut self) -> ExprMatchResult {
        self.expr_bp(0)
    }

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
    fn expr_bp(&mut self, bp: BpVal) -> ExprMatchResult {
        self.min_bp = bp;
        let mut left = self.nud_expr()?;

        loop {
            // while there is an led and the operator's right binding power > min_bp
            // if the current token is a postfix operator or a binary operator, then there is an led
            let is_infix = self.is_infix_op();
            let is_postfix = self.is_postfix_op();
            if !(is_infix || is_postfix) {
                break;
            }

            if self.is_ambiguous_led() {
                return Err(ParseError::AmbiguousExpression);
            }

            // also need to check if rbp is larger than min_bp
            // this will differ based on whether it is infix or postfix, so this must be handled
            // after the if statements
            if is_infix {
                let lbp = infix_op(&self.peek().tok).unwrap().0;
                if lbp < self.min_bp {
                    break;
                }
                left = self.infix_op(left)?;
            } else if is_postfix {
                let lbp = postfix_op(&self.peek().tok).unwrap().0;
                if lbp < self.min_bp {
                    break;
                }
                left = self.postfix_op(left)?;
            }
        }

        Ok(left)
    }
}
