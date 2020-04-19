//! This module implements all of the [Token]s used in the JavaScript programing language.
//!
//! More information:
//!  - [ECMAScript reference][spec]
//!
//! [spec]: https://tc39.es/ecma262/#sec-tokens

use crate::syntax::ast::{keyword::Keyword, pos::Position, punc::Punctuator};
use std::fmt::{Debug, Display, Formatter, Result};

#[cfg(feature = "serde-ast")]
use serde::{Deserialize, Serialize};

/// This represents the smallest individual words, phrases, or characters that JavaScript can understand.
///
/// More information:
///  - [ECMAScript reference][spec]
///
/// [spec]: https://tc39.es/ecma262/#sec-tokens
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The token kind, which contains the actual data of the token.
    pub kind: TokenKind,

    /// The token position from origina source code.
    pub pos: Position,
}

impl Token {
    /// Create a new detailed token from the token data, line number and column number
    pub fn new(kind: TokenKind, line_number: u64, column_number: u64) -> Self {
        Self {
            kind,
            pos: Position::new(line_number, column_number),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.kind)
    }
}

/// A continuous sequence of tokens.
pub struct VecToken(Vec<Token>);

impl Debug for VecToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut buffer = String::new();
        for token in &self.0 {
            buffer.push_str(&token.to_string());
        }
        write!(f, "{}", buffer)
    }
}

/// Represents the type differenct types of numeric literals.
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum NumericLiteral {
    /// A floating point number
    Rational(f64),

    /// An integer
    Integer(i32),
    // TODO: Add BigInt
}

impl From<f64> for NumericLiteral {
    fn from(n: f64) -> Self {
        Self::Rational(n)
    }
}

impl From<i32> for NumericLiteral {
    fn from(n: i32) -> Self {
        Self::Integer(n)
    }
}

/// Represents the type of Token and the data it has inside.
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Clone, PartialEq, Debug)]
pub enum TokenKind {
    /// A boolean literal, which is either `true` or `false`.
    BooleanLiteral(bool),

    /// The end of the file.
    EOF,

    /// An identifier.
    Identifier(String),

    /// A keyword.
    ///
    /// see: [`Keyword`](../keyword/enum.Keyword.html)
    Keyword(Keyword),

    /// A `null` literal.
    NullLiteral,

    /// A numeric literal.
    NumericLiteral(NumericLiteral),

    /// A piece of punctuation
    ///
    /// see: [`Punctuator`](../punc/enum.Punctuator.html)
    Punctuator(Punctuator),

    /// A string literal.
    StringLiteral(String),

    /// A regular expression, consisting of body and flags.
    RegularExpressionLiteral(String, String),

    /// Indicates the end of a line (`\n`).
    LineTerminator,
}

impl TokenKind {
    /// Creates a `NumericLiteral` token type.
    pub fn numeric_literal<L>(lit: L) -> Self
    where
        L: Into<NumericLiteral>,
    {
        Self::NumericLiteral(lit.into())
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match *self {
            TokenKind::BooleanLiteral(ref val) => write!(f, "{}", val),
            TokenKind::EOF => write!(f, "end of file"),
            TokenKind::Identifier(ref ident) => write!(f, "{}", ident),
            TokenKind::Keyword(ref word) => write!(f, "{}", word),
            TokenKind::NullLiteral => write!(f, "null"),
            TokenKind::NumericLiteral(NumericLiteral::Rational(num)) => write!(f, "{}", num),
            TokenKind::NumericLiteral(NumericLiteral::Integer(num)) => write!(f, "{}", num),
            TokenKind::Punctuator(ref punc) => write!(f, "{}", punc),
            TokenKind::StringLiteral(ref lit) => write!(f, "{}", lit),
            TokenKind::RegularExpressionLiteral(ref body, ref flags) => {
                write!(f, "/{}/{}", body, flags)
            }
            TokenKind::LineTerminator => write!(f, "line terminator"),
        }
    }
}
