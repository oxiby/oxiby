use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command as ProcessCommand;

use ariadne::{Color, Label, Report, ReportKind, sources};
use clap::builder::PathBufValueParser;
use clap::{Arg, ArgAction, Command};

use crate::error::ErrorWithSource;
use crate::stage::{check, lex, parse, parse_all};

pub enum CliError {
    Message(String),
    Source(Vec<ErrorWithSource>),
}

impl CliError {
    fn message<M>(message: &M) -> Self
    where
        M: ToString + ?Sized,
    {
        Self::Message(message.to_string())
    }
}

struct Build {
    stdout: bool,
    no_std: bool,
    check: bool,
    entry_file: PathBuf,
    output_dir: PathBuf,
}

impl Build {
    fn should_write_to_stdout(&self) -> bool {
        self.stdout
    }

    fn entry_file_parent(&self) -> Option<&Path> {
        self.entry_file.parent()
    }

    fn create_build_dir(&self) -> Result<(), CliError> {
        if self.output_dir.exists() {
            if !self.output_dir.is_dir() {
                return Err(CliError::message("Output path must be a directory."));
            }
        } else {
            std::fs::create_dir(&self.output_dir).map_err(|_| {
                CliError::message(&format!(
                    "Failed to create output directory: {}",
                    self.output_dir.display()
                ))
            })?;
        }

        Ok(())
    }

    fn output_file(&self, input_file: &Path) -> PathBuf {
        let mut output_file = self
            .entry_file_parent()
            .map_or(input_file, |parent| {
                input_file.strip_prefix(parent).unwrap_or(input_file)
            })
            .to_path_buf();

        output_file.set_extension("rb");

        self.output_dir.join(output_file)
    }
}

struct Check {
    entry_file: PathBuf,
    debug: bool,
}

pub fn run() -> Result<(), CliError> {
    let matches = app().get_matches();

    match matches.subcommand() {
        Some(("check", sub_matches)) => {
            run_check(&Check {
                entry_file: sub_matches
                    .get_one::<PathBuf>("input")
                    .ok_or_else(|| CliError::message("Couldn't determine input path."))?
                    .clone(),
                debug: sub_matches.get_flag("debug"),
            })?;
        }
        Some(("build", sub_matches)) => {
            dbg!(matches.get_flag("no-check"));

            run_build(&Build {
                stdout: sub_matches.get_flag("stdout"),
                no_std: sub_matches.get_flag("no_std"),
                check: !sub_matches.get_flag("no-check"),
                entry_file: sub_matches
                    .get_one::<PathBuf>("input")
                    .ok_or_else(|| CliError::message("Couldn't determine input path."))?
                    .clone(),
                output_dir: sub_matches
                    .get_one::<PathBuf>("output")
                    .ok_or_else(|| CliError::message("Couldn't determine output path."))?
                    .clone(),
            })?;
        }
        Some(("run", sub_matches)) => {
            run_run(&Build {
                stdout: false,
                no_std: sub_matches.get_flag("no_std"),
                check: !sub_matches.get_flag("no-check"),
                entry_file: sub_matches
                    .get_one::<PathBuf>("input")
                    .ok_or_else(|| CliError::message("Couldn't determine input path."))?
                    .clone(),
                output_dir: sub_matches
                    .get_one::<PathBuf>("output")
                    .ok_or_else(|| CliError::message("Couldn't determine output path."))?
                    .clone(),
            })?;
        }
        Some(("clean", sub_matches)) => {
            run_clean(
                sub_matches
                    .get_one::<PathBuf>("output")
                    .ok_or_else(|| CliError::message("Couldn't determine output path."))?
                    .clone(),
            )?;
        }
        Some(("new", sub_matches)) => {
            run_new(
                sub_matches
                    .get_one::<PathBuf>("path")
                    .ok_or_else(|| CliError::message("Couldn't determine output path."))?
                    .clone(),
            )?;
        }
        Some(("lex", sub_matches)) => {
            run_lex(&Build {
                stdout: true,
                no_std: true,
                check: false,
                entry_file: sub_matches
                    .get_one::<PathBuf>("input")
                    .ok_or_else(|| CliError::message("Couldn't determine input path."))?
                    .clone(),
                output_dir: PathBuf::new(),
            })?;
        }
        Some(("parse", sub_matches)) => {
            run_parse(&Build {
                stdout: true,
                no_std: true,
                check: false,
                entry_file: sub_matches
                    .get_one::<PathBuf>("input")
                    .ok_or_else(|| CliError::message("Couldn't determine input path."))?
                    .clone(),
                output_dir: PathBuf::new(),
            })?;
        }
        _ => unreachable!("Clap ensures a subcommand is present"),
    }

    Ok(())
}

pub fn report_errors(errors: Vec<ErrorWithSource>) -> Result<(), std::io::Error> {
    for error_with_source in errors {
        let mut report = Report::build(
            ReportKind::Error,
            (
                error_with_source.file_name_string(),
                error_with_source.error().span.into_range(),
            ),
        )
        .with_message(error_with_source.error().message.clone());

        for help in &error_with_source.error().help {
            report.add_help(help);
        }

        for note in &error_with_source.error().notes {
            report.add_note(note);
        }

        if let Some(detail) = &error_with_source.error().detail {
            report.add_label(
                Label::new((
                    error_with_source.file_name_string(),
                    error_with_source.error().span.into_range(),
                ))
                .with_message(detail)
                .with_color(Color::Red),
            );
        }

        report
            .with_labels(
                error_with_source
                    .error()
                    .contexts
                    .iter()
                    .map(|error_context| {
                        Label::new((
                            error_with_source.file_name_string(),
                            error_context.span.into_range(),
                        ))
                        .with_message(&error_context.message)
                        .with_color(Color::Yellow)
                        .with_order(1)
                    }),
            )
            .finish()
            .eprint(sources([(
                error_with_source.file_name_string(),
                error_with_source.source(),
            )]))?;
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
        .subcommand(command_new())
        .subcommand(command_lex())
        .subcommand(command_parse())
}

fn command_check() -> Command {
    Command::new("check")
        .about("Type checks an Oxiby program")
        .arg(
            Arg::new("debug")
                .short('d')
                .long("debug")
                .action(ArgAction::SetTrue)
                .help("Print the state of the type checker"),
        )
        .arg(arg_input())
}

fn command_build() -> Command {
    Command::new("build")
        .about("Builds an Oxiby program")
        .arg(
            Arg::new("stdout")
                .short('s')
                .long("stdout")
                .action(ArgAction::SetTrue)
                .help("Write program output to stdout instead of a file"),
        )
        .arg(arg_no_std())
        .arg(arg_check())
        .arg(arg_input())
        .arg(arg_output())
}

fn command_run() -> Command {
    Command::new("run")
        .about("Builds and runs an Oxiby program")
        .arg(arg_no_std())
        .arg(arg_check())
        .arg(arg_input())
        .arg(arg_output())
}

fn command_clean() -> Command {
    Command::new("clean")
        .about("Removes the output directory and its contents")
        .arg(arg_output())
}

fn command_new() -> Command {
    Command::new("new")
        .about("Creates a new Oxiby project")
        .arg(
            Arg::new("path")
                .required(true)
                .value_name("PATH")
                .help("Path where the new project should be created")
                .value_parser(PathBufValueParser::new()),
        )
}

fn command_lex() -> Command {
    Command::new("lex")
        .about("Lexes an Oxiby source file and produces tokens")
        .arg(arg_input())
}

fn command_parse() -> Command {
    Command::new("parse")
        .about("Parses an Oxiby source file and produces an abstract syntax tree")
        .arg(arg_input())
}

fn arg_no_std() -> Arg {
    Arg::new("no_std")
        .long("no-std")
        .action(ArgAction::SetTrue)
        .help("Don't build the standard library")
}

fn arg_check() -> Arg {
    Arg::new("no-check")
        .long("no-check")
        .action(ArgAction::SetTrue)
        .help("Don't type check the program before building")
}

fn arg_input() -> Arg {
    Arg::new("input")
        .value_name("INPUT")
        .help("Path to an Oxiby source file")
        .value_parser(PathBufValueParser::new())
        .default_value("src/main.ob")
}

fn arg_output() -> Arg {
    Arg::new("output")
        .value_name("OUTPUT")
        .help("Path to directory to write Ruby source code")
        .value_parser(PathBufValueParser::new())
        .default_value("build")
}

fn run_check(chk: &Check) -> Result<(), CliError> {
    let source = read_file(&chk.entry_file)?;
    let mut modules = HashMap::new();

    modules = check(&source, &chk.entry_file, chk.entry_file.parent(), modules)
        .map_err(CliError::Source)?;

    if chk.debug {
        dbg!(modules);
    }

    Ok(())
}

fn run_build(build: &Build) -> Result<(), CliError> {
    build.create_build_dir()?;

    if !build.no_std {
        crate::compile_std(&build.output_dir)
            .map_err(|errs| CliError::message(&errs.join(", ")))?;
    }

    let source = read_file(&build.entry_file)?;
    let mut modules = HashMap::new();

    if build.check {
        modules = check(
            &source,
            &build.entry_file,
            build.entry_file.parent(),
            modules,
        )
        .map_err(CliError::Source)?;
    } else {
        parse_all(
            &source,
            &build.entry_file,
            build.entry_file.parent(),
            &mut modules,
            true,
        )
        .map_err(CliError::Source)?;
    }

    for (module_path, items) in modules {
        let output = crate::compile_module(module_path.clone(), &items, false);

        if build.should_write_to_stdout() {
            println!("{output}");
        } else {
            let output_file = build.output_file(&module_path.to_path_buf());

            if let Some(parent) = output_file.parent() {
                std::fs::create_dir_all(parent).map_err(|error| CliError::message(&error))?;
            }

            std::fs::write(&output_file, output.strip_suffix('\n').unwrap_or(&output)).map_err(
                |error| {
                    CliError::message(&format!(
                        "trying to write module to {}: {}",
                        output_file.display(),
                        error,
                    ))
                },
            )?;
        }
    }

    Ok(())
}

fn run_run(build: &Build) -> Result<(), CliError> {
    run_build(build)?;

    let mut child = ProcessCommand::new("ruby")
        .arg(build.output_file(&build.entry_file))
        .spawn()
        .map_err(|error| CliError::message(&error))?;

    child.wait().map_err(|error| CliError::message(&error))?;

    Ok(())
}

fn run_clean(output: PathBuf) -> Result<(), CliError> {
    if output.exists() {
        std::fs::remove_dir_all(output).map_err(|error| CliError::message(&error))
    } else {
        Ok(())
    }
}

fn run_new(mut path: PathBuf) -> Result<(), CliError> {
    let base_path = path.clone();

    if base_path.exists() {
        return Err(CliError::message(&format!(
            "Destination `{}` already exists.",
            base_path.display()
        )));
    }

    path.push("src");

    std::fs::create_dir_all(&path).map_err(|error| CliError::message(&error))?;

    path.push("main.ob");

    std::fs::write(&path, "fn main() {\n}".as_bytes())
        .map_err(|error| CliError::message(&error))?;

    println!(
        "New Oxiby project created at path `{}`.",
        base_path.display()
    );

    Ok(())
}

fn run_lex(build: &Build) -> Result<(), CliError> {
    let source = read_file(&build.entry_file)?;
    let tokens = lex(&source, &build.entry_file)
        .map_err(CliError::Source)?
        .into_iter()
        .map(|spanned_token| (spanned_token.inner, spanned_token.span))
        .collect::<Vec<_>>();
    let output = format!("{tokens:#?}");

    println!("{}", output.trim_end());

    Ok(())
}

fn run_parse(build: &Build) -> Result<(), CliError> {
    let source = read_file(&build.entry_file)?;
    let ast = parse(&source, &build.entry_file).map_err(CliError::Source)?;
    let output = format!("{ast:#?}");

    println!("{}", output.trim_end());

    Ok(())
}

fn read_file(file_path: &Path) -> Result<String, CliError> {
    std::fs::read_to_string(file_path)
        .map_err(|_| CliError::Message(format!("Failed to read path: {}", file_path.display())))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_app() {
        app().debug_assert();
    }
}
