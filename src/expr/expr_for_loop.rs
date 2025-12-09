use chumsky::input::MappedInput;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use super::ExprBlock;
use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::expr::Expr;
use crate::pattern::Pattern;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprForLoop {
    pattern: Pattern,
    items: Box<Expr>,
    block: ExprBlock,
    pub(crate) span: SimpleSpan,
}

impl ExprForLoop {
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
        just(Token::For)
            .ignore_then(Pattern::parser(expr.clone(), false))
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .then(ExprBlock::parser(expr))
            .map_with(|((pattern, items), block), extra| Self {
                pattern,
                items: Box::new(items),
                block,
                span: extra.span(),
            })
            .labelled("for loop")
            .as_context()
    }
}

impl WriteRuby for ExprForLoop {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment("(");
        self.items.write_ruby(scope);
        scope.fragment(").each do |");
        match &self.pattern {
            Pattern::Type(pattern_type) => pattern_type.write_ruby(scope),
            Pattern::Ident(pattern_ident) => pattern_ident.write_ruby(scope),
            Pattern::Tuple(pattern_tuple) => pattern_tuple.write_ruby(scope),
            _ => todo!("pattern not yet implemented for for loops"),
        }
        scope.fragment("|");
        scope.block(|scope| {
            scope.newline();
            self.block.unscoped().write_ruby(scope);
        });
        scope.fragment("end");
    }
}

impl Infer for ExprForLoop {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        let element_type = match self.items.infer(checker, context)? {
            ref items_type @ check::Type::Generic(ref constructor_type, ref generic_types) => {
                let name = constructor_type.base_name();
                if name == "List" {
                    vec![generic_types[0].clone()]
                } else if name == "Map" {
                    vec![generic_types[0].clone(), generic_types[1].clone()]
                } else {
                    return Err(Error::build("Type not iterable")
                        .with_detail(
                            &format!("Type `{items_type}` is not iterable."),
                            self.items.span(),
                        )
                        .with_help("Try surrounding the expression with `[` and `]`.")
                        .finish());
                }
            }
            items_type => {
                return Err(Error::build("Type not iterable")
                    .with_detail(
                        &format!("Type `{items_type}` is not iterable."),
                        self.items.span(),
                    )
                    .with_help("Try surrounding the expression with `[` and `]`.")
                    .finish());
            }
        };

        context.push_scope();

        match &self.pattern {
            Pattern::Ident(pattern_ident) => {
                context.push_term_var(pattern_ident.ident.as_str(), element_type[0].clone());
            }
            Pattern::Tuple(pattern_tuple) => match &pattern_tuple.patterns.as_slice() {
                [Pattern::Ident(key_ident), Pattern::Ident(value_ident)] => {
                    context.push_term_var(key_ident.ident.as_str(), element_type[0].clone());
                    context.push_term_var(value_ident.ident.as_str(), element_type[1].clone());
                }
                _ => todo!("Unsupported tuple pattern: {:?}", pattern_tuple),
            },
            pattern => {
                todo!("Type checking for for loops is not yet implemented for pattern {pattern:?}")
            }
        }

        let ty = self.block.infer(checker, context)?;

        context.pop_scope();

        Ok(ty)
    }
}
