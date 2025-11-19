use std::path::{Path, PathBuf};
use std::process::Command as ProcessCommand;

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::Parser;
use clap::builder::{EnumValueParser, PathBufValueParser, PossibleValue};
use clap::{Arg, ArgAction, Command, ValueEnum};

use crate::import::OxibyModulePath;

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

    fn input_path_file_name(&self) -> Result<PathBuf, String> {
        self.input_path
            .file_name()
            .map(|name| Path::new(name).to_path_buf())
            .ok_or_else(|| "Could not create path for input file name".to_string())
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

    fn output_file_path(&self) -> Result<PathBuf, String> {
        if let Some(input_file_name) = self.input_path.file_name() {
            let mut output_file_path = PathBuf::new();
            output_file_path.set_file_name(input_file_name);
            output_file_path.set_extension("rb");

            Ok(output_file_path)
        } else {
            Err("Could not determine file name of input.".to_string())
        }
    }

    fn output_path(&self) -> Result<PathBuf, String> {
        Ok(self.output_dir_path.join(&self.output_file_path()?))
    }

    fn source(&self) -> Result<String, String> {
        std::fs::read_to_string(&self.input_path)
            .map_err(|_| format!("Failed to read path: {}", self.input_path_string()))
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

    let source = build.source()?;

    if build.mode == Mode::Program && !build.no_std {
        crate::compile_std(&build.output_dir_path).map_err(|errs| errs.join(", "))?;
    }

    if let Ok(output) = compile(&source, &build.input_path_file_name()?, build.mode) {
        if build.should_write_to_stdout() {
            println!("{}", output.trim_end());
        } else {
            std::fs::write(
                build.output_path()?,
                output.strip_suffix('\n').unwrap_or(&output),
            )
            .map_err(|err| err.to_string())?;
        }
    }

    Ok(())
}

fn run_run(build: &Build) -> Result<(), String> {
    run_build(build)?;

    let mut child = ProcessCommand::new("ruby")
        .arg(build.output_path()?)
        .spawn()
        .map_err(|error| error.to_string())?;

    child.wait().map_err(|error| error.to_string())?;

    Ok(())
}

fn run_clean(output: PathBuf) -> Result<(), String> {
    std::fs::remove_dir_all(output).map_err(|error| error.to_string())
}

fn compile(source: &str, input_path: &Path, mode: Mode) -> Result<String, String> {
    let oxiby_module_path: OxibyModulePath = input_path.try_into()?;
    let input_path_string = input_path.to_string_lossy().into_owned();

    let (tokens, lex_errors) = crate::lexer().parse(source).into_output_errors();

    let mut output = None;

    let parse_errors = if let Some(tokens) = &tokens {
        if mode == Mode::Tokens {
            return Ok(format!("{tokens:#?}"));
        }

        let (maybe_ast, parse_errors) = crate::parser(crate::make_input)
            .parse(crate::make_input((0..source.len()).into(), tokens))
            .into_output_errors();

        if let Some(ast) = maybe_ast {
            if mode == Mode::Ast {
                return Ok(format!("{ast:#?}"));
            }

            output = Some(crate::compile_module(oxiby_module_path, &ast, false));
        }

        parse_errors
    } else {
        Vec::new()
    };

    if let Some(output) = output
        && lex_errors.is_empty()
        && parse_errors.is_empty()
    {
        return Ok(output);
    }

    lex_errors
        .into_iter()
        .map(|error| error.map_token(|token| token.to_string()))
        .chain(
            parse_errors
                .into_iter()
                .map(|error| error.map_token(|token| token.to_string())),
        )
        .for_each(|error| {
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
            .eprint(sources([(input_path_string.clone(), source)]))
            .unwrap();
        });

    Err("TODO: Under what cirumcstances does this appear".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_app() {
        app().debug_assert();
    }
}
