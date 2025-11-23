use oxiby::CliError;

fn main() {
    if let Err(error) = oxiby::run() {
        if let CliError::Message(message) = error {
            eprintln!("ERROR: {}", message);
        }

        std::process::exit(1);
    }
}
