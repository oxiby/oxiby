use std::fmt::Display;

use chumsky::error::Rich;
use chumsky::span::SimpleSpan;

pub struct Error {
    pub(crate) message: String,
    pub(crate) detail: Option<String>,
    pub(crate) help: Vec<String>,
    pub(crate) notes: Vec<String>,
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
            help: Vec::new(),
            notes: Vec::new(),
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
    detail: Option<String>,
    help: Vec<String>,
    notes: Vec<String>,
    contexts: Vec<ErrorContext>,
    span: SimpleSpan,
}

impl Builder {
    pub fn new<M>(message: &M) -> Self
    where
        M: ToString + ?Sized,
    {
        Builder {
            message: message.to_string(),
            detail: None,
            help: Vec::new(),
            notes: Vec::new(),
            contexts: Vec::new(),
            span: (0..0).into(),
        }
    }

    pub fn finish(self) -> Error {
        Error {
            message: self.message,
            detail: self.detail,
            help: self.help,
            notes: self.notes,
            contexts: self.contexts,
            span: self.span,
        }
    }

    pub fn with_detail<D>(mut self, detail: &D, span: SimpleSpan) -> Self
    where
        D: ToString + ?Sized,
    {
        self.detail = Some(detail.to_string());
        self.span = span;

        self
    }

    pub fn with_help<H>(mut self, help: &H) -> Self
    where
        H: ToString + ?Sized,
    {
        self.help.push(help.to_string());

        self
    }

    pub fn with_note<N>(mut self, note: &N) -> Self
    where
        N: ToString + ?Sized,
    {
        self.notes.push(note.to_string());

        self
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
