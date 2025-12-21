use std::collections::HashSet;
use std::fmt::Display;
use std::path::{Path, PathBuf};

use chumsky::span::SimpleSpan;

use crate::expr::ExprIdent;
use crate::item::Item;

#[derive(Clone, Debug)]
pub struct Module {
    path: ModulePath,
    source: String,
    items: Vec<Item>,
    closures: HashSet<SimpleSpan>,
}

impl Module {
    pub fn new(path: ModulePath, source: String, items: Vec<Item>) -> Self {
        Self {
            path,
            source,
            items,
            closures: HashSet::new(),
        }
    }

    pub fn module_path(&self) -> &ModulePath {
        &self.path
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn items(&self) -> &[Item] {
        &self.items
    }

    pub fn into_parts(self) -> (String, Vec<Item>, HashSet<SimpleSpan>) {
        (self.source, self.items, self.closures)
    }

    pub fn is_entry_module(&self) -> bool {
        self.path.is_entry_module()
    }

    pub fn is_std(&self) -> bool {
        self.path.is_std()
    }

    pub fn extend_closures(&mut self, closures: HashSet<SimpleSpan>) {
        self.closures.extend(closures);
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path)
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct ModulePath {
    path_components: Vec<String>,
    is_entry_module: bool,
}

impl ModulePath {
    pub fn len(&self) -> usize {
        self.path_components.len()
    }

    pub fn parts(&self) -> &[String] {
        &self.path_components
    }

    pub fn is_entry_module(&self) -> bool {
        self.is_entry_module
    }

    pub fn is_std(&self) -> bool {
        self.path_components
            .first()
            .is_some_and(|path_component| path_component == "std")
    }

    pub fn set_is_entry_module(&mut self, value: bool) {
        self.is_entry_module = value;
    }

    pub fn to_path_buf(&self) -> PathBuf {
        self.parts().join("/").into()
    }
}

impl Display for ModulePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path_components.join("."))
    }
}

impl From<&[&str]> for ModulePath {
    fn from(value: &[&str]) -> Self {
        Self {
            path_components: value.iter().map(ToString::to_string).collect(),
            is_entry_module: false,
        }
    }
}

impl From<&str> for ModulePath {
    fn from(value: &str) -> Self {
        Self {
            path_components: value.split('.').map(ToString::to_string).collect(),
            is_entry_module: false,
        }
    }
}

impl TryFrom<&Path> for ModulePath {
    type Error = String;

    fn try_from(value: &Path) -> Result<Self, Self::Error> {
        let mut path_buf = value.to_path_buf();
        path_buf.set_extension("");
        let modules_str = path_buf.to_str().ok_or_else(|| {
            format!(
                "Bad path `{}` for Oxiby source file. Paths must be valid unicode.",
                value.display()
            )
        })?;
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
                    return Err(format!(
                        "Bad path `{}` for Oxiby source file. Paths must have only components \
                         beginning with the lowercase ASCII alphabet or an underscore.",
                        value.display()
                    ));
                }
            }

            if !part.chars().all(|chr| {
                chr == '/' || chr == '_' || chr.is_ascii_digit() || chr.is_ascii_lowercase()
            }) {
                return Err(format!(
                    "Bad path `{}` for Oxiby source file. Paths must consist only of the \
                     lowercase ASCII alphabet, digits 0-9, underscores, and directory separators.",
                    value.display()
                ));
            }
        }

        Ok(Self {
            path_components: parts,
            is_entry_module: false,
        })
    }
}

impl From<Vec<ExprIdent>> for ModulePath {
    fn from(value: Vec<ExprIdent>) -> Self {
        Self {
            path_components: value.iter().map(ToString::to_string).collect(),
            is_entry_module: false,
        }
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

impl From<ModulePath> for RubyModuleConstants {
    fn from(value: ModulePath) -> Self {
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

    use super::{ModulePath, RubyModuleConstants};

    #[test]
    fn path_to_oxiby_to_ruby() {
        let path = Path::new("examples/tic_tac_toe");
        let oxiby: ModulePath = path.try_into().unwrap();
        let actual: RubyModuleConstants = oxiby.into();

        let expected = "Examples::TicTacToe";

        assert_eq!(actual.to_string(), expected);
    }
}
