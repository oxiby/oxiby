use std::fmt::Display;

use chumsky::error::Rich;
use chumsky::span::SimpleSpan;

pub struct Error {
    pub(crate) message: String,
    pub(crate) detail: Option<String>,
    pub(crate) contexts: Vec<ErrorContext>,
    pub(crate) span: SimpleSpan,
}

pub struct ErrorContext {
    pub(crate) message: String,
    pub(crate) span: SimpleSpan,
}

impl Error {
    pub fn message<M>(message: &M) -> Self
    where
        M: ToString,
    {
        Self {
            message: message.to_string(),
            detail: None,
            contexts: Vec::new(),
            span: (0..0).into(),
        }
    }

    pub fn spanned_message<M>(message: &M, span: SimpleSpan) -> Self
    where
        M: ToString,
    {
        Self {
            message: message.to_string(),
            detail: None,
            contexts: Vec::new(),
            span,
        }
    }
}

impl<'a, T> From<Rich<'a, T, SimpleSpan>> for Error
where
    T: Display,
{
    fn from(value: Rich<'a, T, SimpleSpan>) -> Self {
        Self {
            message: value.to_string(),
            detail: Some(value.reason().to_string()),
            contexts: value
                .contexts()
                .map(|(label, span)| ErrorContext {
                    message: format!("while parsing this {label}"),
                    span: *span,
                })
                .collect(),
            span: *value.span(),
        }
    }
}
