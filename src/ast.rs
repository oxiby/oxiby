use std::fmt::Display;

use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::Spanned;
use crate::expr::ExprIdent;
use crate::item::{Item, ItemEnum, ItemFn, ItemImpl, ItemStruct, ItemTrait, ItemUse};
use crate::token::Token;
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct Record<'a> {
    pub name: ExprIdent<'a>,
    pub ty: Type<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Visibility {
    Private,
    Public,
}

impl Visibility {
    pub fn parser<'a, I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        just(Token::Pub)
            .or_not()
            .map(|maybe_pub| match maybe_pub {
                Some(_) => Visibility::Public,
                None => Visibility::Private,
            })
            .labelled("visibility")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TupleField<'a> {
    pub visibility: Visibility,
    pub ty: Type<'a>,
}

impl<'a> TupleField<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        Visibility::parser()
            .then(Type::parser())
            .map(|(visibility, ty)| Self { visibility, ty })
            .labelled("field")
            .as_context()
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct RecordField<'a> {
    pub visibility: Visibility,
    pub name: ExprIdent<'a>,
    pub ty: Type<'a>,
}

impl<'a> RecordField<'a> {
    pub fn parser<I>()
    -> impl Parser<'a, I, Self, extra::Err<Rich<'a, Token<'a>, SimpleSpan>>> + Clone
    where
        I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    {
        Visibility::parser()
            .then(ExprIdent::parser())
            .then_ignore(just(Token::Colon))
            .then(Type::parser())
            .map(|((visibility, name), ty)| Self {
                visibility,
                name,
                ty,
            })
            .labelled("named field")
            .as_context()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    Assign,
    AddAssign,
    DivAssign,
    MultAssign,
    SubAssign,
    And,
    Or,
    Eq,
    Ne,
    Not,
    LtEq,
    GtEq,
    Lt,
    Gt,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let variant = match self {
            Self::Assign => "=",
            Self::AddAssign => "+=",
            Self::DivAssign => "/=",
            Self::MultAssign => "*=",
            Self::SubAssign => "-=",
            Self::And => "&&",
            Self::Or => "||",
            Self::Eq => "==",
            Self::Ne => "!=",
            Self::Not => "!",
            Self::LtEq => "<=",
            Self::GtEq => ">=",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::Add => "+",
            Self::Sub => "-",
        };

        write!(f, "{variant}")
    }
}

#[must_use]
pub fn parser<'a, I, M>(
    make_input: M,
) -> impl Parser<'a, I, Vec<Item<'a>>, extra::Err<Rich<'a, Token<'a>>>>
where
    I: BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan>,
    M: Fn(SimpleSpan, &'a [Spanned<Token<'a>>]) -> I + Clone + 'a,
{
    choice((
        ItemEnum::parser(make_input.clone()).map(Item::Enum).boxed(),
        ItemFn::parser(make_input.clone(), false)
            .map(Item::Fn)
            .boxed(),
        ItemImpl::parser(make_input.clone()).map(Item::Impl).boxed(),
        ItemStruct::parser(make_input.clone())
            .map(Item::Struct)
            .boxed(),
        ItemTrait::parser(make_input).map(Item::Trait).boxed(),
        ItemUse::parser().map(Item::Use).boxed(),
    ))
    .labelled("item")
    .as_context()
    .repeated()
    .collect::<Vec<Item>>()
    .boxed()
}

#[must_use]
pub fn make_input<'a>(
    eoi: SimpleSpan,
    tokens: &'a [Spanned<Token<'a>>],
) -> impl BorrowInput<'a, Token = Token<'a>, Span = SimpleSpan> {
    tokens.map(eoi, |(token, span)| (token, span))
}
