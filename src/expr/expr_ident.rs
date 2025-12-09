use std::fmt::Display;

use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::compiler::{Scope, WriteRuby};
use crate::token::Token;
use crate::types::TypeIdent;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct ExprIdent {
    pub(crate) ident: String,
    pub(crate) span: SimpleSpan,
}

impl ExprIdent {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        select! {
            Token::TermIdent(ident) = extra => Self { ident, span: extra.span() },
            Token::SelfTerm = extra => Self { ident: "self".to_string(), span: extra.span() },
        }
        .labelled("identifier")
    }

    pub fn as_str(&self) -> &str {
        &self.ident
    }
}

impl Display for ExprIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl WriteRuby for ExprIdent {
    fn write_ruby(&self, scope: &mut Scope) {
        match scope.resolve_ident(&self.ident) {
            Some((path, _kind)) => scope.fragment(path),
            None => scope.fragment(&self.ident),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprTypeIdent {
    ident: TypeIdent,
    pub(crate) span: SimpleSpan,
}

impl ExprTypeIdent {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        TypeIdent::parser().map_with(|ident, extra| Self {
            ident,
            span: extra.span(),
        })
    }

    pub fn as_str(&self) -> &str {
        self.ident.as_str()
    }
}

impl WriteRuby for ExprTypeIdent {
    fn write_ruby(&self, scope: &mut Scope) {
        self.ident.write_ruby(scope);
    }
}
