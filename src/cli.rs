use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command as ProcessCommand;

use ariadne::{Color, Label, Report, ReportKind, sources};
use clap::builder::{EnumValueParser, PathBufValueParser, PossibleValue};
use clap::{Arg, ArgAction, Command, ValueEnum};

use crate::module::{
    Error as ModuleError,
    module_ast,
    module_check,
    module_program,
    module_tokens,
};

pub enum Error {
    Message(String),
    // This means that lexing, parsing, and/or type checking errors were already printed and we
    // only need to exit non-zero.
    Exit,
}

impl From<String> for Error {
    fn from(value: String) -> Self {
        Self::Message(value)
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum Mode {
    Ast,
    Program,
    Tokens,
}

impl ValueEnum for Mode {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::Ast, Self::Program, Self::Tokens]
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        Some(match self {
            Self::Ast => PossibleValue::new("ast").help("Parser output"),
            Self::Program => PossibleValue::new("program").help("Ruby source code"),
            Self::Tokens => PossibleValue::new("tokens").help("Lexer output"),
        })
    }
}

struct Build {
    mode: Mode,
    stdout: bool,
    no_std: bool,
    entry_file: PathBuf,
    output_dir: PathBuf,
}

impl Build {
    fn should_write_to_stdout(&self) -> bool {
        self.stdout || self.mode != Mode::Program
    }

    fn entry_file_string(&self) -> String {
        self.entry_file.to_string_lossy().into_owned()
    }

    fn entry_file_parent(&self) -> Option<&Path> {
        self.entry_file.parent()
    }

    fn create_build_dir(&self) -> Result<(), String> {
        if self.output_dir.exists() {
            if !self.output_dir.is_dir() {
                return Err("Output path must be a directory.".to_string());
            }
        } else {
            std::fs::create_dir(&self.output_dir).map_err(|_| {
                format!(
                    "Failed to create output directory: {}",
                    self.output_dir.display()
                )
            })?;
        }

        Ok(())
    }

    fn output_file(&self, input_file: &Path) -> PathBuf {
        let mut output_file = self
            .entry_file_parent()
            .map_or(input_file, |parent| {
                input_file
                    .strip_prefix(parent)
                    .expect("parent was extracted from the entry file so it should match")
            })
            .to_path_buf();

        output_file.set_extension("rb");

        self.output_dir.join(output_file)
    }
}

pub fn run() -> Result<(), Error> {
    let matches = app().get_matches();

    match matches.subcommand() {
        Some(("check", sub_matches)) => {
            run_check(&Build {
                mode: Mode::Program,
                stdout: true,
                no_std: false,
                entry_file: sub_matches
                    .get_one::<PathBuf>("input")
                    .ok_or_else(|| Error::Message("Couldn't determine input path.".to_string()))?
                    .clone(),
                output_dir: "TODO: Maybe a `Check` type for when `output_dir` isn't needed?".into(),
            })?;
        }
        Some(("build", sub_matches)) => {
            run_build(&Build {
                mode: *sub_matches
                    .get_one::<Mode>("mode")
                    .ok_or_else(|| Error::Message("Couldn't determine mode.".to_string()))?,
                stdout: sub_matches.get_flag("stdout"),
                no_std: sub_matches.get_flag("no_std"),
                entry_file: sub_matches
                    .get_one::<PathBuf>("input")
                    .ok_or_else(|| Error::Message("Couldn't determine input path.".to_string()))?
                    .clone(),
                output_dir: sub_matches
                    .get_one::<PathBuf>("output")
                    .ok_or_else(|| Error::Message("Couldn't determine output path.".to_string()))?
                    .clone(),
            })?;
        }
        Some(("run", sub_matches)) => {
            run_run(&Build {
                mode: Mode::Program,
                stdout: false,
                no_std: sub_matches.get_flag("no_std"),
                entry_file: sub_matches
                    .get_one::<PathBuf>("input")
                    .ok_or_else(|| Error::Message("Couldn't determine input path.".to_string()))?
                    .clone(),
                output_dir: sub_matches
                    .get_one::<PathBuf>("output")
                    .ok_or_else(|| Error::Message("Couldn't determine output path.".to_string()))?
                    .clone(),
            })?;
        }
        Some(("clean", sub_matches)) => {
            run_clean(
                sub_matches
                    .get_one::<PathBuf>("output")
                    .ok_or_else(|| Error::Message("Couldn't determine output path.".to_string()))?
                    .clone(),
            )?;
        }
        _ => unreachable!("Clap ensures a subcommand is present"),
    }

    Ok(())
}

#[must_use]
fn app() -> Command {
    Command::new("obc")
        .version(env!("CARGO_PKG_VERSION"))
        .propagate_version(true)
        .about("The Oxiby compiler")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(command_check())
        .subcommand(command_build())
        .subcommand(command_run())
        .subcommand(command_clean())
}

fn command_check() -> Command {
    Command::new("check")
        .about("Type checks an Oxiby program")
        .arg(arg_input())
}

fn command_build() -> Command {
    Command::new("build")
        .about("Builds an Oxiby program")
        .arg(
            Arg::new("mode")
                .short('m')
                .long("mode")
                .value_name("MODE")
                .help("Type of output to produce")
                .value_parser(EnumValueParser::<Mode>::new())
                .default_value("program"),
        )
        .arg(
            Arg::new("stdout")
                .short('s')
                .long("stdout")
                .action(ArgAction::SetTrue)
                .help("Write program output to stdout instead of a file"),
        )
        .arg(arg_no_std())
        .arg(arg_input())
        .arg(arg_output())
}

fn command_run() -> Command {
    Command::new("run")
        .about("Builds and runs an Oxiby program")
        .arg(arg_no_std())
        .arg(arg_input())
        .arg(arg_output())
}

fn command_clean() -> Command {
    Command::new("clean")
        .about("Removes the output directory and its contents")
        .arg(arg_output())
}
fn arg_no_std() -> Arg {
    Arg::new("no_std")
        .long("no-std")
        .action(ArgAction::SetTrue)
        .help("Don't build the standard library")
}

fn arg_input() -> Arg {
    Arg::new("input")
        .required(true)
        .value_name("INPUT")
        .help("Path to an Oxiby source file")
        .value_parser(PathBufValueParser::new())
}

fn arg_output() -> Arg {
    Arg::new("output")
        .value_name("OUTPUT")
        .help("Path to directory to write Ruby source code")
        .value_parser(PathBufValueParser::new())
        .default_value("build")
}

fn run_check(build: &Build) -> Result<(), Error> {
    let mut program_errors = None;

    if let Err(error) = module_check(&build.entry_file) {
        match error {
            ModuleError::Program(source, errors) => program_errors = Some((source, errors)),
            ModuleError::Message(error) => return Err(Error::Message(error)),
        }
    }

    if let Some((source, errors)) = program_errors {
        let entry_file_string = build.entry_file_string();

        for error in errors {
            Report::build(
                ReportKind::Error,
                (entry_file_string.clone(), error.span().into_range()),
            )
            .with_message(error.to_string())
            .with_label(
                Label::new((entry_file_string.clone(), error.span().into_range()))
                    .with_message(error.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(error.contexts().map(|(label, span)| {
                Label::new((entry_file_string.clone(), span.into_range()))
                    .with_message(format!("while parsing this {label}"))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .eprint(sources([(entry_file_string.clone(), source.clone())]))
            .unwrap();
        }

        return Err(Error::Exit);
    }

    Ok(())
}

fn run_build(build: &Build) -> Result<(), Error> {
    build.create_build_dir()?;

    let mut program_errors = None;

    match build.mode {
        Mode::Tokens | Mode::Ast => {
            let result = if build.mode == Mode::Tokens {
                module_tokens(&build.entry_file)
            } else {
                module_ast(&build.entry_file)
            };

            match result {
                Ok(output) => println!("{}", output.trim_end()),
                Err(error) => match error {
                    crate::module::Error::Program(source, errors) => {
                        program_errors = Some((source, errors));
                    }
                    crate::module::Error::Message(error) => return Err(Error::Message(error)),
                },
            }
        }
        Mode::Program => {
            if !build.no_std {
                crate::compile_std(&build.output_dir).map_err(|errs| errs.join(", "))?;
            }

            let compiled_modules = HashMap::new();

            match module_program(
                &build.entry_file,
                build.entry_file.parent(),
                compiled_modules,
                true,
            ) {
                Ok(compiled_modules) => {
                    for (input_path, output) in compiled_modules {
                        if build.should_write_to_stdout() {
                            println!("{output}");
                        } else {
                            let output_file = build.output_file(&input_path);

                            if let Some(parent) = output_file.parent() {
                                std::fs::create_dir_all(parent)
                                    .map_err(|error| error.to_string())?;
                            }

                            std::fs::write(
                                &output_file,
                                output.strip_suffix('\n').unwrap_or(&output),
                            )
                            .map_err(|error| {
                                format!(
                                    "trying to write module to {}: {}",
                                    output_file.display(),
                                    error,
                                )
                            })?;
                        }
                    }
                }
                Err(error) => match error {
                    ModuleError::Program(source, errors) => {
                        program_errors = Some((source.clone(), errors.clone()));
                    }
                    ModuleError::Message(error) => return Err(Error::Message(error)),
                },
            }
        }
    }

    if let Some((source, errors)) = program_errors {
        let entry_file_string = build.entry_file_string();

        for error in errors {
            Report::build(
                ReportKind::Error,
                (entry_file_string.clone(), error.span().into_range()),
            )
            .with_message(error.to_string())
            .with_label(
                Label::new((entry_file_string.clone(), error.span().into_range()))
                    .with_message(error.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(error.contexts().map(|(label, span)| {
                Label::new((entry_file_string.clone(), span.into_range()))
                    .with_message(format!("while parsing this {label}"))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .eprint(sources([(entry_file_string.clone(), source.clone())]))
            .unwrap();
        }
    }

    Ok(())
}

fn run_run(build: &Build) -> Result<(), Error> {
    run_build(build)?;

    let mut child = ProcessCommand::new("ruby")
        .arg(build.output_file(&build.entry_file))
        .spawn()
        .map_err(|error| error.to_string())?;

    child.wait().map_err(|error| error.to_string())?;

    Ok(())
}

fn run_clean(output: PathBuf) -> Result<(), String> {
    std::fs::remove_dir_all(output).map_err(|error| error.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_app() {
        app().debug_assert();
    }
}
