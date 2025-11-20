use std::path::{Path, PathBuf};
use std::process::Command as ProcessCommand;

use ariadne::{Color, Label, Report, ReportKind, sources};
use clap::builder::{EnumValueParser, PathBufValueParser, PossibleValue};
use clap::{Arg, ArgAction, Command, ValueEnum};

use crate::module::{Error, module_ast, module_program, module_tokens};

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
    input_path: PathBuf,
    output_dir_path: PathBuf,
}

impl Build {
    fn should_write_to_stdout(&self) -> bool {
        self.stdout || self.mode != Mode::Program
    }

    fn input_path_string(&self) -> String {
        self.input_path.to_string_lossy().into_owned()
    }

    fn create_build_dir(&self) -> Result<(), String> {
        if self.output_dir_path.exists() {
            if !self.output_dir_path.is_dir() {
                return Err("Output path must be a directory.".to_string());
            }
        } else {
            std::fs::create_dir(&self.output_dir_path).map_err(|_| {
                format!(
                    "Failed to create output directory: {}",
                    self.output_dir_path.to_string_lossy()
                )
            })?;
        }

        Ok(())
    }

    fn output_file_path(&self, input_path: Option<&Path>) -> Result<PathBuf, String> {
        let maybe_file_name = match input_path {
            Some(input_path) => input_path.file_name(),
            None => self.input_path.file_name(),
        };

        if let Some(input_file_name) = maybe_file_name {
            let mut output_file_path = PathBuf::new();
            output_file_path.set_file_name(input_file_name);
            output_file_path.set_extension("rb");

            Ok(output_file_path)
        } else {
            Err("Could not determine file name of input.".to_string())
        }
    }

    fn output_path(&self, input_path: Option<&Path>) -> Result<PathBuf, String> {
        Ok(self
            .output_dir_path
            .join(&self.output_file_path(input_path)?))
    }
}

pub fn run() -> Result<(), String> {
    let matches = app().get_matches();

    match matches.subcommand() {
        Some(("build", sub_matches)) => {
            run_build(&Build {
                mode: *sub_matches
                    .get_one::<Mode>("mode")
                    .ok_or_else(|| "Couldn't determine mode.".to_string())?,
                stdout: sub_matches.get_flag("stdout"),
                no_std: sub_matches.get_flag("no_std"),
                input_path: sub_matches
                    .get_one::<PathBuf>("input")
                    .ok_or_else(|| "Couldn't determine input path.".to_string())?
                    .clone(),
                output_dir_path: sub_matches
                    .get_one::<PathBuf>("output")
                    .ok_or_else(|| "Couldn't determine output path.".to_string())?
                    .clone(),
            })?;
        }
        Some(("run", sub_matches)) => {
            run_run(&Build {
                mode: Mode::Program,
                stdout: false,
                no_std: sub_matches.get_flag("no_std"),
                input_path: sub_matches
                    .get_one::<PathBuf>("input")
                    .ok_or_else(|| "Couldn't determine input path.".to_string())?
                    .clone(),
                output_dir_path: sub_matches
                    .get_one::<PathBuf>("output")
                    .ok_or_else(|| "Couldn't determine output path.".to_string())?
                    .clone(),
            })?;
        }
        Some(("clean", sub_matches)) => {
            run_clean(
                sub_matches
                    .get_one::<PathBuf>("output")
                    .ok_or_else(|| "Couldn't determine output path.".to_string())?
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
        .subcommand(command_build())
        .subcommand(command_run())
        .subcommand(command_clean())
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

fn run_build(build: &Build) -> Result<(), String> {
    build.create_build_dir()?;

    let mut program_errors = None;

    match build.mode {
        Mode::Tokens | Mode::Ast => {
            let result = if build.mode == Mode::Tokens {
                module_tokens(&build.input_path)
            } else {
                module_ast(&build.input_path)
            };

            match result {
                Ok(output) => println!("{}", output.trim_end()),
                Err(error) => match error {
                    Error::Program(source, errors) => program_errors = Some((source, errors)),
                    Error::Other(error) => return Err(error),
                },
            }
        }
        Mode::Program => {
            if !build.no_std {
                crate::compile_std(&build.output_dir_path).map_err(|errs| errs.join(", "))?;
            }

            let compiled_modules = Vec::new();

            match module_program(&build.input_path, compiled_modules) {
                Ok(compiled_modules) => {
                    for (input_path, output) in compiled_modules {
                        if build.should_write_to_stdout() {
                            println!("{output}");
                        } else {
                            std::fs::write(
                                build.output_path(Some(&input_path))?,
                                output.strip_suffix('\n').unwrap_or(&output),
                            )
                            .map_err(|err| err.to_string())?;
                        }
                    }
                }
                Err(error) => match error {
                    Error::Program(source, errors) => {
                        program_errors = Some((source.clone(), errors.clone()));
                    }
                    Error::Other(error) => return Err(error),
                },
            }
        }
    }

    if let Some((source, errors)) = program_errors {
        let input_path_string = build.input_path_string();

        for error in errors {
            Report::build(
                ReportKind::Error,
                (input_path_string.clone(), error.span().into_range()),
            )
            .with_message(error.to_string())
            .with_label(
                Label::new((input_path_string.clone(), error.span().into_range()))
                    .with_message(error.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(error.contexts().map(|(label, span)| {
                Label::new((input_path_string.clone(), span.into_range()))
                    .with_message(format!("while parsing this {label}"))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .eprint(sources([(input_path_string.clone(), source.clone())]))
            .unwrap();
        }
    }

    Ok(())
}

fn run_run(build: &Build) -> Result<(), String> {
    run_build(build)?;

    let mut child = ProcessCommand::new("ruby")
        .arg(build.output_path(None)?)
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
