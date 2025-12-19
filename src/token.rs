use std::fmt::Display;

use chumsky::prelude::*;

const RUBY_KEYWORDS: &[&str] = &[
    "BEGIN",
    "END",
    "__ENCODING__",
    "__FILE__",
    "__LINE__",
    "alias",
    "and",
    "begin",
    "case",
    "class",
    "def",
    "defined?",
    "do",
    "elsif",
    "end",
    "ensure",
    "module",
    "next",
    "nil",
    "not",
    "or",
    "redo",
    "rescue",
    "retry",
    "super",
    "then",
    "undef",
    "unless",
    "until",
    "when",
    "yield",
];

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Keywords
    Arrow,
    At,
    Break,
    Continue,
    Else,
    Enum,
    Fn,
    For,
    If,
    Impl,
    In,
    Is,
    Let,
    Loop,
    Match,
    Pub,
    Return,
    Ruby(String),
    SelfTerm,
    SelfType,
    Struct,
    Trait,
    Type,
    Use,
    Where,
    While,

    // Literals
    Boolean(bool),
    Float(f64),
    Integer(u64),
    String(Vec<Spanned<Token>>),

    // Workaround for https://github.com/zesterer/chumsky/discussions/891
    StringPartLiteral(String),
    StringPartExpr(Vec<Spanned<Token>>),

    // Identifiers
    TermIdent(String),
    TypeIdent(String),

    // Operators
    Add,
    And,
    AddAssign,
    Assign,
    DivAssign,
    MultAssign,
    SubAssign,
    Div,
    Eq,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Mod,
    Mul,
    Ne,
    Not,
    Or,
    Sub,

    // Delimiters and control
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Colon,
    Semicolon,
    Comma,
    Dot,
    Underscore,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Keywords
            Self::Arrow => write!(f, "->"),
            Self::At => write!(f, "@"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::Else => write!(f, "else"),
            Self::Enum => write!(f, "enum"),
            Self::Fn => write!(f, "fn"),
            Self::For => write!(f, "for"),
            Self::If => write!(f, "if"),
            Self::Impl => write!(f, "impl"),
            Self::In => write!(f, "in"),
            Self::Is => write!(f, "is"),
            Self::Let => write!(f, "let"),
            Self::Loop => write!(f, "loop"),
            Self::Match => write!(f, "match"),
            Self::Pub => write!(f, "pub"),
            Self::Return => write!(f, "return"),
            Self::Ruby(ruby) => write!(f, "ruby {{ {ruby} }}"),
            Self::SelfTerm => write!(f, "self"),
            Self::SelfType => write!(f, "Self"),
            Self::Struct => write!(f, "struct"),
            Self::Trait => write!(f, "trait"),
            Self::Type => write!(f, "type"),
            Self::Use => write!(f, "use"),
            Self::Where => write!(f, "where"),
            Self::While => write!(f, "while"),

            // Literals
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Float(fl) => write!(f, "{fl}"),
            Self::Integer(i) => write!(f, "{i}"),
            Self::String(spanned_tokens) => write!(
                f,
                "{}",
                spanned_tokens
                    .iter()
                    .map(|spanned_token| spanned_token.inner.to_string())
                    .collect::<String>()
            ),

            // Workaround for https://github.com/zesterer/chumsky/discussions/891
            Self::StringPartLiteral(s) => write!(f, "{s}"),
            Self::StringPartExpr(spanned_tokens) => write!(
                f,
                "{}",
                spanned_tokens
                    .iter()
                    .map(|spanned_token| spanned_token.inner.to_string())
                    .collect::<String>()
            ),

            // Identifiers
            Self::TermIdent(i) | Token::TypeIdent(i) => write!(f, "{i}"),

            // Operators
            Self::Add => write!(f, "+"),
            Self::And => write!(f, "&&"),
            Self::Assign => write!(f, "="),
            Self::AddAssign => write!(f, "+="),
            Self::DivAssign => write!(f, "/="),
            Self::MultAssign => write!(f, "*="),
            Self::SubAssign => write!(f, "-="),
            Self::Div => write!(f, "/"),
            Self::Eq => write!(f, "=="),
            Self::Gt => write!(f, ">"),
            Self::GtEq => write!(f, ">="),
            Self::Lt => write!(f, "<"),
            Self::LtEq => write!(f, "<="),
            Self::Mod => write!(f, "%"),
            Self::Mul => write!(f, "*"),
            Self::Ne => write!(f, "!="),
            Self::Not => write!(f, "!"),
            Self::Or => write!(f, "||"),
            Self::Sub => write!(f, "-"),

            // Delimiters
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::Colon => write!(f, ":"),
            Self::Semicolon => write!(f, ";"),
            Self::Comma => write!(f, ","),
            Self::Dot => write!(f, "."),
            Self::Underscore => write!(f, "_"),
        }
    }
}

pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token>>, extra::Err<Rich<'src, char, SimpleSpan>>> {
    let mut token = Recursive::declare();
    let mut string = Recursive::declare();

    let float = text::int(10)
        .then(just('.').then(text::int(10)))
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Float);

    let integer = text::int(10)
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Integer);

    let string_part = choice((
        just("#{")
            .ignore_then(
                token
                    .clone()
                    .and_is(just('{').not()) // These two lines make
                    .and_is(just('}').not()) // all the example inputs work
                    .padded()
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just('}'))
            .map(Token::StringPartExpr),
        any()
            .and_is(just('"').not())
            .and_is(just("#{").not())
            .repeated()
            .at_least(1)
            .to_slice()
            .map(|s: &'src str| Token::StringPartLiteral(s.to_string())),
    ));

    string.define(
        just('"')
            .ignore_then(string_part.spanned().repeated().collect())
            .then_ignore(just('"'))
            .map(Token::String),
    );

    let str_symbol = choice((
        just("->").to(Token::Arrow),
        just("==").to(Token::Eq),
        just("=").to(Token::Assign),
        just("*=").to(Token::MultAssign),
        just("+=").to(Token::AddAssign),
        just("-=").to(Token::SubAssign),
        just("/=").to(Token::DivAssign),
        just("&&").to(Token::And),
        just("||").to(Token::Or),
        just("!=").to(Token::Ne),
        just("!").to(Token::Not),
        just("<=").to(Token::LtEq),
        just(">=").to(Token::GtEq),
        just("<").to(Token::Lt),
        just(">").to(Token::Gt),
        just("*").to(Token::Mul),
        just("/").to(Token::Div),
        just("%").to(Token::Mod),
        just("+").to(Token::Add),
        just("-").to(Token::Sub),
        just("_").to(Token::Underscore),
    ));

    let char_symbol = choice((
        just('(').to(Token::LParen),
        just(')').to(Token::RParen),
        just('[').to(Token::LBracket),
        just(']').to(Token::RBracket),
        just('{').to(Token::LBrace),
        just('}').to(Token::RBrace),
        just(':').to(Token::Colon),
        just(';').to(Token::Semicolon),
        just(',').to(Token::Comma),
        just('.').to(Token::Dot),
        just('@').to(Token::At),
    ));

    let ruby = just("ruby")
        .ignored()
        .padded()
        .then_ignore(just('{'))
        .then(
            any()
                .and_is(just('}').not())
                .padded()
                .repeated()
                .to_slice()
                .map(ToString::to_string),
        )
        .then_ignore(just('}'))
        .map(|((), code)| Token::Ruby(code));

    let ident = text::ascii::ident()
        .validate(|ident: &str, extra, emitter| {
            if RUBY_KEYWORDS.contains(&ident) {
                emitter.emit(Rich::custom(
                    extra.span(),
                    format!("`{ident}` is a reserved keyword"),
                ));
            }

            ident
        })
        .map(|ident: &str| match ident {
            // Keywords
            "->" => Token::Arrow,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "else" => Token::Else,
            "enum" => Token::Enum,
            "fn" => Token::Fn,
            "for" => Token::For,
            "if" => Token::If,
            "impl" => Token::Impl,
            "in" => Token::In,
            "is" => Token::Is,
            "let" => Token::Let,
            "loop" => Token::Loop,
            "match" => Token::Match,
            "pub" => Token::Pub,
            "return" => Token::Return,
            "self" => Token::SelfTerm,
            "Self" => Token::SelfType,
            "struct" => Token::Struct,
            "trait" => Token::Trait,
            "type" => Token::Type,
            "use" => Token::Use,
            "where" => Token::Where,
            "while" => Token::While,

            // Literals
            "false" => Token::Boolean(false),
            "true" => Token::Boolean(true),

            ident => {
                if let Some(first_character) = ident.chars().next()
                    && first_character.is_ascii_uppercase()
                {
                    Token::TypeIdent(ident.to_string())
                } else {
                    Token::TermIdent(ident.to_string())
                }
            }
        });

    token.define(choice((
        ruby.spanned(),
        float.spanned(),
        integer.spanned(),
        string.spanned(),
        str_symbol.spanned(),
        char_symbol.spanned(),
        ident.spanned(),
    )));

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    // Ensure `!=` becomes `Token::Ne` rather than consuming the `!` by itself
    #[test]
    fn lex_ne() {
        assert_eq!(
            lexer()
                .parse("!=")
                .unwrap()
                .into_iter()
                .map(|spanned| spanned.inner.to_string())
                .collect::<Vec<_>>(),
            vec!["!="]
        );
    }
}
