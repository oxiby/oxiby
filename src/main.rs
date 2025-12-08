use oxiby::CliError;

fn main() {
    if let Err(error) = oxiby::run() {
        match error {
            CliError::Message(message) => eprintln!("ERROR: {}", message),
            CliError::Source(errors) => {
                if let Err(error) = oxiby::report_errors(errors) {
                    eprintln!(
                        "ERROR: I/O error while trying to report compiler errors: {}",
                        error
                    );
                }
            }
        }

        std::process::exit(1);
    }
}
