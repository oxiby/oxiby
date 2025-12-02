use std::fmt::Display;
use std::path::Path;

use crate::expr::ExprIdent;

#[derive(Debug, Clone, PartialEq)]
pub struct OxibyModulePath(Vec<String>);

impl OxibyModulePath {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn parts(&self) -> &[String] {
        &self.0
    }
}

impl Display for OxibyModulePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.join(""))
    }
}

impl From<&[&str]> for OxibyModulePath {
    fn from(value: &[&str]) -> Self {
        Self(value.iter().map(ToString::to_string).collect())
    }
}

impl TryFrom<&Path> for OxibyModulePath {
    type Error = String;

    fn try_from(value: &Path) -> Result<Self, Self::Error> {
        let mut path_buf = value.to_path_buf();
        path_buf.set_extension("");
        let modules_str = path_buf
            .to_str()
            .ok_or_else(|| "Path was not valid unicode.".to_string())?;
        let parts = modules_str
            .split('/')
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>();

        for (index, part) in parts.iter().enumerate() {
            if index == 0 {
                let first = part
                    .chars()
                    .nth(0)
                    .ok_or_else(|| "Path contained empty component.".to_string())?;

                if !(first == '_' || first.is_ascii_lowercase()) {
                    return Err(
                        "Oxiby source files must have paths components beginning with the \
                         lowercase ASCII alphabet or an underscore."
                            .to_string(),
                    );
                }
            }

            if !part.chars().all(|chr| {
                chr == '/' || chr == '_' || chr.is_ascii_digit() || chr.is_ascii_lowercase()
            }) {
                return Err(
                    "Oxiby source files must have paths consisting only of the lowercase ASCII \
                     alphabet, digits 0-9, underscores, and directory separators."
                        .to_string(),
                );
            }
        }

        Ok(Self(parts))
    }
}

impl From<Vec<ExprIdent<'_>>> for OxibyModulePath {
    fn from(value: Vec<ExprIdent<'_>>) -> Self {
        Self(value.iter().map(ToString::to_string).collect())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RubyModuleConstants(Vec<String>);

impl Display for RubyModuleConstants {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.join("::"))
    }
}

impl RubyModuleConstants {
    pub fn as_slice(&self) -> &[String] {
        self.0.as_slice()
    }
}

impl From<OxibyModulePath> for RubyModuleConstants {
    fn from(value: OxibyModulePath) -> Self {
        #[derive(PartialEq)]
        enum State {
            Capitalize,
            Verbatim,
        }

        let mut constants = Vec::with_capacity(value.len());

        for part in value.parts() {
            let mut constant = String::new();
            let mut state = State::Capitalize;

            for chr in part.chars() {
                if chr == '_' {
                    state = State::Capitalize;
                } else if state == State::Capitalize {
                    constant.push(chr.to_ascii_uppercase());
                    state = State::Verbatim;
                } else {
                    constant.push(chr);
                }
            }

            constants.push(constant);
        }

        Self(constants)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::{OxibyModulePath, RubyModuleConstants};

    #[test]
    fn path_to_oxiby_to_ruby() {
        let path = Path::new("examples/tic_tac_toe");
        let oxiby: OxibyModulePath = path.try_into().unwrap();
        let actual: RubyModuleConstants = oxiby.into();

        let expected = "Examples::TicTacToe";

        assert_eq!(actual.to_string(), expected);
    }
}
