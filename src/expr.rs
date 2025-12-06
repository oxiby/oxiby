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
mod expr_block;
mod expr_boolean;
mod expr_break;
mod expr_call;
mod expr_closure;
mod expr_conditional;
mod expr_continue;
mod expr_enum;
mod expr_float;
mod expr_for_loop;
mod expr_ident;
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
mod expr_while_loop;

pub use expr_array::expr_array_parser;
pub use expr_block::ExprBlock;
pub use expr_boolean::ExprBoolean;
pub use expr_break::ExprBreak;
pub use expr_call::ExprCall;
pub use expr_closure::ExprClosure;
pub use expr_conditional::ExprConditional;
pub use expr_continue::ExprContinue;
pub use expr_enum::ExprEnum;
pub use expr_float::ExprFloat;
pub use expr_for_loop::ExprForLoop;
pub use expr_ident::{ExprIdent, ExprTypeIdent};
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
pub use expr_struct::ExprStruct;
pub use expr_tuple::ExprTuple;
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
    fn infer(&self, checker: &Checker, context: &mut Context) -> Result<check::Type, Error> {
        let ty = match self {
            // Literals
            Expr::Boolean(..) => check::Type::boolean(),
            Expr::Float(..) => check::Type::float(),
            Expr::Integer(..) => check::Type::integer(),
            Expr::String(..) => check::Type::string(),
            Expr::Range(..) => check::Type::range(),

            // Compound primitives
            Expr::Map(expr_map) => expr_map.infer(checker, context)?,
            Expr::List(expr_list) => expr_list.infer(checker, context)?,
            Expr::Tuple(expr_tuple) => expr_tuple.infer(checker, context)?,

            // Identifiers
            Expr::ExprIdent(ident) => context.find(ident.as_str(), self.span())?,
            Expr::TypeIdent(expr_type_ident) => {
                let name = expr_type_ident.as_str();

                match checker.type_constructors.get(name) {
                    Some((ty, _)) => match ty {
                        check::Type::Constructor(_) => ty.clone(),
                        _ => todo!(
                            "Under what circumstances is Expr::TypeIdent inferred when it's a \
                             tuple/record struct?"
                        ),
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
            Expr::Field(expr_field) => expr_field.infer(checker, context)?,

            // Calls
            Expr::Call(expr_call) => expr_call.infer(checker, context)?,

            // Control flow
            Expr::Break(expr_break) => expr_break.infer(checker, context)?,
            Expr::Conditional(expr_conditional) => expr_conditional.infer(checker, context)?,
            Expr::Continue(..) => check::Type::unit(),
            Expr::ForLoop(expr_for_loop) => expr_for_loop.infer(checker, context)?,
            Expr::Loop(expr_loop) => expr_loop.infer(checker, context)?,
            Expr::Return(expr_return) => expr_return.infer(checker, context)?,
            Expr::WhileLoop(expr_while) => expr_while.infer(checker, context)?,

            // Patterns
            Expr::Let(expr_let) => expr_let.infer(checker, context)?,

            // Misc.
            Expr::Binary(expr_binary) => expr_binary.infer(checker, context)?,

            _ => todo!("Type inference not yet implemented for expression {self:?}"),
        };

        Ok(ty)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary<'a> {
    pub(crate) op: Operator,
    pub(crate) lhs: Box<Expr<'a>>,
    pub(crate) rhs: Box<Expr<'a>>,
    pub(crate) span: SimpleSpan,
}

impl WriteRuby for ExprBinary<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        self.lhs.write_ruby(scope);
        scope.fragment(format!(" {} ", self.op));
        self.rhs.write_ruby(scope);
    }
}

impl Infer for ExprBinary<'_> {
    fn infer(&self, checker: &Checker, context: &mut Context) -> Result<check::Type, Error> {
        let lhs_type = self.lhs.infer(checker, context)?;
        let rhs_type = self.rhs.infer(checker, context)?;

        // For now, assume that lhs implements the operator and that rhs is the appropriate type.
        Ok(match &self.op {
            Operator::Assign => rhs_type,
            Operator::AddAssign
            | Operator::DivAssign
            | Operator::MultAssign
            | Operator::SubAssign => lhs_type,
            Operator::And
            | Operator::Or
            | Operator::Eq
            | Operator::Ne
            | Operator::LtEq
            | Operator::GtEq
            | Operator::Lt
            | Operator::Gt => check::Type::boolean(),
            Operator::Mul | Operator::Div | Operator::Mod | Operator::Add | Operator::Sub => {
                check::Type::integer()
            }
            Operator::Not => unreachable!("Cannot have a binary `Not` expression."),
        })
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnary<'a> {
    op: Operator,
    expr: Box<Expr<'a>>,
    span: SimpleSpan,
}

impl WriteRuby for ExprUnary<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        scope.fragment(self.op.to_string());
        self.expr.write_ruby(scope);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprField<'a> {
    lhs: Box<Expr<'a>>,
    rhs: Box<Expr<'a>>,
    span: SimpleSpan,
}

impl WriteRuby for ExprField<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        let self_receiver =
            matches!(*self.lhs, Expr::ExprIdent(ref expr_ident) if expr_ident.as_str() == "self");

        if !self_receiver {
            self.lhs.write_ruby(scope);
            scope.fragment(".");
        }

        if let Expr::Integer(_) = *self.rhs {
            scope.fragment("__");
        }

        self.rhs.write_ruby(scope);
    }
}

impl Infer for ExprField<'_> {
    fn infer(&self, checker: &Checker, context: &mut Context) -> Result<check::Type, Error> {
        let ty: check::Type = if let Expr::TypeIdent(ref expr_type_ident) = *self.lhs {
            let Some((lhs_ty, members)) = checker.type_constructors.get(expr_type_ident.as_str())
            else {
                return Err(Error::build("Unknown type")
                    .with_detail(
                        &format!("Type `{}` is not in scope.", expr_type_ident.as_str()),
                        expr_type_ident.span,
                    )
                    .with_help("You might need to import this type from another module.")
                    .finish());
            };

            if let Expr::Call(ref expr_call) = *self.rhs {
                let name = expr_call.name.as_str();

                let Some(rhs_ty) = members.functions.get(name) else {
                    return Err(Error::build("Unknown method")
                        .with_detail(
                            &format!("Type `{lhs_ty}` does not have a method `{name}`.",),
                            expr_call.span,
                        )
                        .finish());
                };

                let check::Type::Fn(function) = rhs_ty else {
                    return Err(Error::type_mismatch()
                        .with_detail(
                            &format!(
                                "Value `{}` is of type `{lhs_ty}` but is being called as a \
                                 function.",
                                rhs_ty.name()
                            ),
                            self.span,
                        )
                        .finish());
                };

                if !function.is_static {
                    return Err(Error::build("Invalid static method call")
                        .with_detail(
                            &format!(
                                "`{name}` is being called as a static method, but it is a \
                                 instance method."
                            ),
                            self.rhs.span(),
                        )
                        .with_help(&format!(
                            "Try calling the method on a value of type `{lhs_ty}`."
                        ))
                        .finish());
                }

                expr_call.infer_method(checker, context, function)?
            } else {
                todo!(
                    "TODO: Inference for fields where `lhs` is a type identifier and `rhs` isn't \
                     a call."
                );
            }
        } else if let Expr::ExprIdent(ref expr_ident) = *self.lhs {
            let lhs_ty = context.find(expr_ident.as_str(), expr_ident.span)?;

            let (_, members) = checker.type_constructors.get(&lhs_ty.name()).expect(
                "Should always exist because we were able to find the type in the context.",
            );

            if let Expr::Call(ref expr_call) = *self.rhs {
                let name = expr_call.name.as_str();

                let Some(rhs_ty) = members.functions.get(name) else {
                    return Err(Error::build("Unknown method")
                        .with_detail(
                            &format!("Type `{lhs_ty}` does not have a method `{name}`.",),
                            expr_call.span,
                        )
                        .finish());
                };

                let check::Type::Fn(function) = rhs_ty else {
                    return Err(Error::type_mismatch()
                        .with_detail(
                            &format!(
                                "Value `{}` is of type `{rhs_ty}` but is being called as a \
                                 function.",
                                rhs_ty.name()
                            ),
                            self.span,
                        )
                        .finish());
                };

                if function.is_static {
                    return Err(Error::build("Invalid method call")
                        .with_detail(
                            &format!(
                                "`{name}` is being called as an instance method, but it is a \
                                 static method."
                            ),
                            self.rhs.span(),
                        )
                        .with_help(&format!("Try using the syntax `{lhs_ty}.{name}(...)`."))
                        .finish());
                }
                expr_call.infer_method(checker, context, function)?
            } else {
                todo!(
                    "TODO: Inference for fields where `lhs` is an expression identifier and `rhs` \
                     isn't a call."
                );
            }
        } else {
            todo!("TODO: Inference for fields where `lhs` isn't an identifier.");
        };

        Ok(ty)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprIndex<'a> {
    expr: Box<Expr<'a>>,
    index: Box<Expr<'a>>,
    span: SimpleSpan,
}

impl WriteRuby for ExprIndex<'_> {
    fn write_ruby(&self, scope: &mut Scope) {
        self.expr.write_ruby(scope);
        scope.fragment("[");
        self.index.write_ruby(scope);
        scope.fragment("]");
    }
}
