fn main() {
    if let Err(error) = oxiby::run() {
        eprintln!("ERROR: {:?}", error);

        std::process::exit(1);
    }
}
