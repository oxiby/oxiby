use chumsky::input::BorrowInput;
use chumsky::pratt::{Operator as _, infix, left, postfix, prefix};
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;

use crate::Spanned;
use crate::ast::Operator;
use crate::check::{self, Checker, Context, Infer};
use crate::compiler::{Scope, WriteRuby};
use crate::error::Error;
use crate::token::Token;

mod expr_array;
mod expr_binary;
mod expr_block;
mod expr_boolean;
mod expr_break;
mod expr_call;
mod expr_closure;
mod expr_conditional;
mod expr_continue;
mod expr_enum;
mod expr_field;
mod expr_float;
mod expr_for_loop;
mod expr_ident;
mod expr_index;
mod expr_integer;
mod expr_let;
mod expr_list;
mod expr_loop;
mod expr_map;
mod expr_match;
mod expr_parenthesized;
mod expr_range;
mod expr_return;
mod expr_ruby;
mod expr_string;
mod expr_struct;
mod expr_tuple;
mod expr_unary;
mod expr_while_loop;

pub use expr_array::expr_array_parser;
pub use expr_binary::ExprBinary;
pub use expr_block::ExprBlock;
pub use expr_boolean::ExprBoolean;
pub use expr_break::ExprBreak;
pub use expr_call::{ExprCall, Noun, infer_function};
pub use expr_closure::ExprClosure;
pub use expr_conditional::ExprConditional;
pub use expr_continue::ExprContinue;
pub use expr_enum::ExprEnum;
pub use expr_field::ExprField;
pub use expr_float::ExprFloat;
pub use expr_for_loop::ExprForLoop;
pub use expr_ident::{ExprIdent, ExprTypeIdent};
pub use expr_index::ExprIndex;
pub use expr_integer::ExprInteger;
pub use expr_let::ExprLet;
pub use expr_list::ExprList;
pub use expr_loop::ExprLoop;
pub use expr_map::ExprMap;
pub use expr_match::ExprMatch;
pub use expr_parenthesized::ExprParenthesized;
pub use expr_range::ExprRange;
pub use expr_return::ExprReturn;
pub use expr_ruby::ExprRuby;
pub use expr_string::ExprString;
pub use expr_struct::{ExprStruct, check_records};
pub use expr_tuple::ExprTuple;
pub use expr_unary::ExprUnary;
pub use expr_while_loop::ExprWhileLoop;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    // Literals
    Boolean(ExprBoolean),
    Integer(ExprInteger),
    Float(ExprFloat),
    String(ExprString<'a>),
    Range(ExprRange<'a>),

    // Compound primitives
    Map(ExprMap<'a>),
    List(ExprList<'a>),
    Tuple(ExprTuple<'a>),

    // Data structures
    Struct(ExprStruct<'a>),
    Enum(ExprEnum<'a>),

    // Identifiers
    ExprIdent(ExprIdent<'a>),
    TypeIdent(ExprTypeIdent<'a>),

    // Member access
    Field(ExprField<'a>),
    Index(ExprIndex<'a>),

    // Calls
    Call(ExprCall<'a>),
    Closure(ExprClosure<'a>),

    // Control flow
    Break(ExprBreak<'a>),
    Conditional(ExprConditional<'a>),
    Continue(ExprContinue),
    ForLoop(ExprForLoop<'a>),
    Loop(ExprLoop<'a>),
    Return(ExprReturn<'a>),
    WhileLoop(ExprWhileLoop<'a>),

    // Patterns
    Let(ExprLet<'a>),
    Match(ExprMatch<'a>),

    // Misc.
    Block(ExprBlock<'a>),
    Unary(ExprUnary<'a>),
    Binary(ExprBinary<'a>),
    Parenthesized(ExprParenthesized<'a>),
    Ruby(ExprRuby<'a>),
}

impl<'a> Expr<'a> {
    pub fn parser<I, M>(
        make_input: M,
    ) -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
        M: Fn(SimpleSpan, &'a [Spanned<Token<'a>>]) -> I + Clone + 'a,
    {
        recursive(|expr| {
            choice((
                ExprCall::parser(expr.clone()).map(Expr::Call).boxed(), // must come before ExprIdent
                ExprBoolean::parser().map(Expr::Boolean).boxed(),
                ExprInteger::parser().map(Expr::Integer).boxed(),
                ExprFloat::parser().map(Expr::Float).boxed(),
                ExprString::parser(expr.clone(), make_input.clone())
                    .map(Expr::String)
                    .boxed(),
                ExprRange::parser(expr.clone()).map(Expr::Range).boxed(),
                expr_array_parser(expr.clone()).boxed(),
                ExprTuple::parser(expr.clone()).map(Expr::Tuple).boxed(),
                ExprEnum::parser(expr.clone()).map(Expr::Enum).boxed(),
                ExprStruct::parser(expr.clone()).map(Expr::Struct).boxed(),
                ExprIdent::parser().map(Expr::ExprIdent).boxed(),
                ExprTypeIdent::parser().map(Expr::TypeIdent).boxed(),
                ExprClosure::parser(expr.clone()).map(Expr::Closure).boxed(),
                ExprBreak::parser(expr.clone()).map(Expr::Break).boxed(),
                ExprConditional::parser(expr.clone())
                    .map(Expr::Conditional)
                    .boxed(),
                ExprContinue::parser().map(Expr::Continue).boxed(),
                ExprForLoop::parser(expr.clone(), make_input.clone())
                    .map(Expr::ForLoop)
                    .boxed(),
                ExprLoop::parser(expr.clone()).map(Expr::Loop).boxed(),
                ExprReturn::parser(expr.clone()).map(Expr::Return).boxed(),
                ExprWhileLoop::parser(expr.clone())
                    .map(Expr::WhileLoop)
                    .boxed(),
                ExprLet::parser(expr.clone(), make_input.clone())
                    .map(Expr::Let)
                    .boxed(),
                ExprMatch::parser(expr.clone(), make_input)
                    .map(Expr::Match)
                    .boxed(),
                ExprBlock::parser(expr.clone()).map(Expr::Block).boxed(),
                ExprParenthesized::parser(expr.clone())
                    .map(Expr::Parenthesized)
                    .boxed(),
                ExprRuby::parser().map(Expr::Ruby).boxed(),
            ))
            .boxed()
            .pratt((
                prefix(
                    3,
                    choice((
                        just(Token::Sub).to(Operator::Sub),
                        just(Token::Not).to(Operator::Not),
                    ))
                    .labelled("operator"),
                    |op, rhs: Expr, extra| {
                        Expr::Unary(ExprUnary {
                            op,
                            expr: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                postfix(
                    3,
                    expr.delimited_by(just(Token::LBracket), just(Token::RBracket)),
                    |lhs, op, extra| {
                        Expr::Index(ExprIndex {
                            expr: Box::new(lhs),
                            index: Box::new(op),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::Dot).labelled("operator"),
                    |lhs: Expr, _, mut rhs, extra| {
                        // Let the call know it isn't a free function because that changes how
                        // imports are resolved.
                        if let Self::Call(ref mut expr_call) = rhs {
                            expr_call.set_field();
                        }

                        Expr::Field(ExprField {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::Assign).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::Assign,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::AddAssign).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::AddAssign,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::DivAssign).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::DivAssign,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::MultAssign).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::MultAssign,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::SubAssign).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::SubAssign,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::And).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::And,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::Or).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::Or,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::Eq).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::Eq,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::Ne).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::Ne,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::LtEq).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::LtEq,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::GtEq).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::GtEq,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::Lt).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::Lt,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::Gt).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::Gt,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::Mul).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::Mul,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::Div).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::Div,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::Mod).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::Mod,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::Add).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::Add,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
                infix(
                    left(2),
                    just(Token::Sub).labelled("operator"),
                    |lhs: Expr, _, rhs, extra| {
                        Expr::Binary(ExprBinary {
                            op: Operator::Sub,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            span: extra.span(),
                        })
                    },
                )
                .boxed(),
            ))
            .boxed()
        })
        .labelled("expression")
        .as_context()
        .boxed()
    }

    pub fn span(&self) -> SimpleSpan {
        match self {
            Self::Boolean(expr_boolean) => expr_boolean.span,
            Self::Integer(expr_integer) => expr_integer.span,
            Self::Float(expr_float) => expr_float.span,
            Self::String(expr_string) => expr_string.span,
            Self::Range(expr_range) => expr_range.span,
            Self::Map(expr_map) => expr_map.span,
            Self::List(expr_list) => expr_list.span,
            Self::Tuple(expr_tuple) => expr_tuple.span,
            Self::Struct(expr_struct) => expr_struct.span,
            Self::Enum(expr_enum) => expr_enum.span,
            Self::ExprIdent(expr_ident) => expr_ident.span,
            Self::TypeIdent(expr_type_ident) => expr_type_ident.span,
            Self::Field(expr_field) => expr_field.span,
            Self::Index(expr_index) => expr_index.span,
            Self::Call(expr_call) => expr_call.span,
            Self::Closure(expr_closure) => expr_closure.span,
            Self::Break(expr_break) => expr_break.span,
            Self::Conditional(expr_conditional) => expr_conditional.span,
            Self::Continue(expr_continue) => expr_continue.span,
            Self::ForLoop(expr_for_loop) => expr_for_loop.span,
            Self::Loop(expr_loop) => expr_loop.span,
            Self::Return(expr_return) => expr_return.span,
            Self::WhileLoop(expr_while_loop) => expr_while_loop.span,
            Self::Let(expr_let) => expr_let.span,
            Self::Match(expr_match) => expr_match.span,
            Self::Block(expr_block) => expr_block.span,
            Self::Unary(expr_unary) => expr_unary.span,
            Self::Binary(expr_binary) => expr_binary.span,
            Self::Parenthesized(expr_parenthesized) => expr_parenthesized.span,
            Self::Ruby(expr_ruby) => expr_ruby.span,
        }
    }
}

impl WriteRuby for Expr<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        match self {
            Self::Boolean(expr_boolean) => expr_boolean.write_ruby(scope),
            Self::Integer(expr_integer) => expr_integer.write_ruby(scope),
            Self::Float(expr_float) => expr_float.write_ruby(scope),
            Self::String(expr_string) => expr_string.write_ruby(scope),
            Self::Range(expr_range) => expr_range.write_ruby(scope),
            Self::Map(expr_map) => expr_map.write_ruby(scope),
            Self::List(expr_list) => expr_list.write_ruby(scope),
            Self::Tuple(expr_tuple) => expr_tuple.write_ruby(scope),
            Self::Struct(expr_struct) => expr_struct.write_ruby(scope),
            Self::Enum(expr_enum) => expr_enum.write_ruby(scope),
            Self::ExprIdent(expr_ident) => expr_ident.write_ruby(scope),
            Self::TypeIdent(expr_type_ident) => expr_type_ident.write_ruby(scope),
            Self::Field(expr_field) => expr_field.write_ruby(scope),
            Self::Index(expr_index) => expr_index.write_ruby(scope),
            Self::Call(expr_call) => expr_call.write_ruby(scope),
            Self::Closure(expr_closure) => expr_closure.write_ruby(scope),
            Self::Break(expr_break) => expr_break.write_ruby(scope),
            Self::Conditional(expr_conditional) => expr_conditional.write_ruby(scope),
            Self::Continue(expr_continue) => expr_continue.write_ruby(scope),
            Self::ForLoop(expr_for_loop) => expr_for_loop.write_ruby(scope),
            Self::Loop(expr_loop) => expr_loop.write_ruby(scope),
            Self::Return(expr_return) => expr_return.write_ruby(scope),
            Self::WhileLoop(expr_while_loop) => expr_while_loop.write_ruby(scope),
            Self::Let(expr_let) => expr_let.write_ruby(scope),
            Self::Match(expr_match) => expr_match.write_ruby(scope),
            Self::Block(expr_block) => expr_block.write_ruby(scope),
            Self::Unary(expr_unary) => expr_unary.write_ruby(scope),
            Self::Binary(expr_binary) => expr_binary.write_ruby(scope),
            Self::Parenthesized(expr_parenthesized) => expr_parenthesized.write_ruby(scope),
            Self::Ruby(expr_ruby) => expr_ruby.write_ruby(scope),
        }
    }
}

impl Infer for Expr<'_> {
    fn infer(&self, checker: &mut Checker, context: &mut Context) -> Result<check::Type, Error> {
        let ty = match self {
            // Literals
            Self::Boolean(..) => check::Type::boolean(),
            Self::Float(..) => check::Type::float(),
            Self::Integer(..) => check::Type::integer(),
            Self::String(..) => check::Type::string(),
            Self::Range(..) => check::Type::range(),

            // Compound primitives
            Self::Map(expr_map) => expr_map.infer(checker, context)?,
            Self::List(expr_list) => expr_list.infer(checker, context)?,
            Self::Tuple(expr_tuple) => expr_tuple.infer(checker, context)?,

            // Data structures
            Self::Struct(expr_struct) => expr_struct.infer(checker, context)?,
            Self::Enum(expr_enum) => expr_enum.infer(checker, context)?,

            // Identifiers
            Self::ExprIdent(ident) => context.find(ident.as_str(), self.span())?,
            Self::TypeIdent(expr_type_ident) => {
                let name = expr_type_ident.as_str();

                match checker.type_constructors.get(name) {
                    Some((ty, members)) => match ty {
                        check::Type::Constructor(_) if members.value_constructors.is_empty() => {
                            ty.clone()
                        }
                        _ => {
                            return Err(Error::build("Invalid struct literal")
                                .with_detail(
                                    &format!(
                                        "Struct `{name}` is not a unit struct and cannot be \
                                         constructed with the syntax `{name}`."
                                    ),
                                    expr_type_ident.span,
                                )
                                .with_help(
                                    &(if members.value_constructors.contains_key(name) {
                                        format!("Try using tuple struct syntax: `{ty}(...)`")
                                    } else {
                                        format!("Try using record struct syntax: `{ty} {{ ... }}`")
                                    }),
                                )
                                .finish());
                        }
                    },
                    None => {
                        return Err(Error::build("Unknown type")
                            .with_detail(
                                &format!("Type `{name}` is not in scope."),
                                expr_type_ident.span,
                            )
                            .with_help("You might need to import this type from another module.")
                            .finish());
                    }
                }
            }

            // Member access
            Self::Field(expr_field) => expr_field.infer(checker, context)?,

            // Calls
            Self::Call(expr_call) => expr_call.infer(checker, context)?,
            Self::Index(expr_index) => expr_index.infer(checker, context)?,

            // Control flow
            Self::Break(expr_break) => expr_break.infer(checker, context)?,
            Self::Conditional(expr_conditional) => expr_conditional.infer(checker, context)?,
            Self::Continue(..) => check::Type::unit(),
            Self::ForLoop(expr_for_loop) => expr_for_loop.infer(checker, context)?,
            Self::Loop(expr_loop) => expr_loop.infer(checker, context)?,
            Self::Return(expr_return) => expr_return.infer(checker, context)?,
            Self::WhileLoop(expr_while) => expr_while.infer(checker, context)?,

            // Patterns
            Self::Let(expr_let) => expr_let.infer(checker, context)?,

            // Misc.
            Self::Block(expr_block) => expr_block.infer(checker, context)?,
            Self::Unary(expr_unary) => expr_unary.infer(checker, context)?,
            Self::Binary(expr_binary) => expr_binary.infer(checker, context)?,
            Self::Parenthesized(expr_parenthesized) => {
                expr_parenthesized.infer(checker, context)?
            }

            _ => todo!("Type inference not yet implemented for expression {self:?}"),
        };

        Ok(ty)
    }
}
