use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::check::{self, Checker, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::pattern::{Pattern, match_bindings};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprLet {
    pub(crate) pattern: Pattern,
    pub(crate) body: Box<Expr>,
    pub(crate) span: SimpleSpan,
}

impl ExprLet {
    pub fn parser<'a>(
        expr: impl Parser<
            'a,
            MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
            Expr,
            extra::Err<Rich<'a, Token, SimpleSpan>>,
        > + Clone
        + 'a,
    ) -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        just(Token::Let)
            .ignore_then(Pattern::parser(expr.clone(), false))
            .then_ignore(just(Token::Assign))
            .then(expr)
            .map_with(|(pattern, body), extra| Self {
                pattern,
                body: Box::new(body),
                span: extra.span(),
            })
            .labelled("let")
            .as_context()
    }

    fn write_body(&self, scope: &mut Scope) {
        if let Expr::TypeIdent(expr_type_ident) = &*self.body {
            scope.fragment(format!("{}.new", expr_type_ident.as_str()));
        } else {
            self.body.write_ruby(scope);
        }
    }
}

impl WriteRuby for ExprLet {
    fn write_ruby(&self, scope: &mut Scope) {
        match &self.pattern {
            Pattern::Type(pattern_type) => {
                pattern_type.write_ruby(scope);
                scope.fragment(" = ");
                self.write_body(scope);
            }
            Pattern::Ident(pattern_ident) => {
                pattern_ident.write_ruby(scope);
                scope.fragment(" = ");
                self.write_body(scope);
            }
            Pattern::Tuple(pattern_tuple) => {
                pattern_tuple.write_ruby(scope);
                scope.fragment(" = ");
                self.write_body(scope);
            }
            Pattern::Ctor(pattern_ctor) => {
                self.write_body(scope);
                scope.fragment(" => ");
                pattern_ctor.write_ruby(scope);
            }
            _ => todo!("pattern not yet implemented for let bindings"),
        }
    }
}

impl Infer for ExprLet {
    fn infer(&self, checker: &mut Checker) -> Result<check::Type, Error> {
        let expr_ty = self.body.infer(checker)?;

        let bindings = match_bindings(
            checker,
            &expr_ty,
            &self.pattern,
            self.span,
            self.body.span(),
            self.pattern.span(),
        )?;

        for (name, ty) in bindings {
            checker.push_term_var(name, ty);
        }

        Ok(expr_ty)
    }
}
