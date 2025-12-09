use std::fmt::Display;

use chumsky::input::MappedInput;
use chumsky::prelude::*;

use crate::expr::ExprIdent;
use crate::item::{Item, ItemEnum, ItemFn, ItemImpl, ItemStruct, ItemTrait, ItemUse};
use crate::token::Token;
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct Record {
    pub name: ExprIdent,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Visibility {
    Private,
    Public,
}

impl Visibility {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
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
pub struct TupleField {
    pub visibility: Visibility,
    pub ty: Type,
}

impl TupleField {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
        Visibility::parser()
            .then(Type::parser())
            .map(|(visibility, ty)| Self { visibility, ty })
            .labelled("field")
            .as_context()
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct RecordField {
    pub visibility: Visibility,
    pub name: ExprIdent,
    pub ty: Type,
}

impl RecordField {
    pub fn parser<'a>() -> impl Parser<
        'a,
        MappedInput<'a, Token, SimpleSpan, &'a [Spanned<Token>]>,
        Self,
        extra::Err<Rich<'a, Token, SimpleSpan>>,
    > + Clone {
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
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
pub fn parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    MappedInput<'tokens, Token, SimpleSpan, &'tokens [Spanned<Token>]>,
    Vec<Item>,
    extra::Err<Rich<'tokens, Token>>,
> {
    choice((
        ItemEnum::parser().map(Item::Enum).boxed(),
        ItemFn::parser(false).map(Item::Fn).boxed(),
        ItemImpl::parser().map(Item::Impl).boxed(),
        ItemStruct::parser().map(Item::Struct).boxed(),
        ItemTrait::parser().map(Item::Trait).boxed(),
        ItemUse::parser().map(Item::Use).boxed(),
    ))
    .labelled("item")
    .as_context()
    .repeated()
    .collect::<Vec<Item>>()
    .boxed()
}
