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
    pub fn build<M>(message: &M) -> Builder
    where
        M: ToString + ?Sized,
    {
        Builder::new(message)
    }

    pub fn message<M>(message: &M) -> Self
    where
        M: ToString + ?Sized,
    {
        Self {
            message: message.to_string(),
            detail: None,
            contexts: Vec::new(),
            span: (0..0).into(),
        }
    }

    pub fn type_mismatch() -> Builder {
        Builder::new("Mismatched types")
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

pub struct Builder {
    message: String,
}

impl Builder {
    pub fn new<M>(message: &M) -> Self
    where
        M: ToString + ?Sized,
    {
        Builder {
            message: message.to_string(),
        }
    }

    pub fn finish(self) -> Error {
        Error {
            message: self.message,
            detail: None,
            contexts: Vec::new(),
            span: (0..0).into(),
        }
    }

    pub fn detail<D>(self, detail: &D, span: SimpleSpan) -> DetailedBuilder
    where
        D: ToString + ?Sized,
    {
        DetailedBuilder {
            message: self.message,
            detail: detail.to_string(),
            span,
        }
    }
}

pub struct DetailedBuilder {
    message: String,
    detail: String,
    span: SimpleSpan,
}

impl DetailedBuilder {
    pub fn finish(self) -> Error {
        Error {
            message: self.message,
            detail: Some(self.detail),
            contexts: Vec::new(),
            span: self.span,
        }
    }

    pub fn into_contextual(self) -> ContextBuilder {
        ContextBuilder {
            message: self.message,
            detail: self.detail,
            contexts: Vec::new(),
            span: self.span,
        }
    }

    pub fn with_context<M>(self, message: &M, span: SimpleSpan) -> ContextBuilder
    where
        M: ToString + ?Sized,
    {
        ContextBuilder {
            message: self.message,
            detail: self.detail,
            contexts: vec![ErrorContext {
                message: message.to_string(),
                span,
            }],
            span: self.span,
        }
    }
}

pub struct ContextBuilder {
    message: String,
    detail: String,
    contexts: Vec<ErrorContext>,
    span: SimpleSpan,
}

impl ContextBuilder {
    pub fn finish(self) -> Error {
        Error {
            message: self.message,
            detail: Some(self.detail),
            contexts: self.contexts,
            span: self.span,
        }
    }

    pub fn with_context<M>(mut self, message: &M, span: SimpleSpan) -> Self
    where
        M: ToString + ?Sized,
    {
        self.contexts.push(ErrorContext {
            message: message.to_string(),
            span,
        });

        self
    }
}
