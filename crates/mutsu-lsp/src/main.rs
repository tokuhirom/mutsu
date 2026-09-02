//! `mutsu-lsp` — the mutsu language server (ADR-0065).
//!
//! Speaks LSP over stdio, which is what every client and agent harness expects.
//! Diagnostics go to the client; anything the server has to say about itself
//! goes to stderr, because stdout is the protocol channel and a stray `println!`
//! there corrupts the stream.

use std::process::ExitCode;

use lsp_server::Connection;

const USAGE: &str = "\
mutsu-lsp — the language server for mutsu

USAGE:
    mutsu-lsp [--stdio]

The server speaks LSP over stdin/stdout. `--stdio` is accepted because most
clients pass it, and is the only supported transport.

OPTIONS:
    --stdio        Use stdio as the transport (the default).
    -h, --help     Print this help.
    -V, --version  Print the version.
";

fn main() -> ExitCode {
    for arg in std::env::args().skip(1) {
        match arg.as_str() {
            "--stdio" => {}
            "-h" | "--help" => {
                print!("{USAGE}");
                return ExitCode::SUCCESS;
            }
            "-V" | "--version" => {
                println!("mutsu-lsp {}", env!("CARGO_PKG_VERSION"));
                return ExitCode::SUCCESS;
            }
            other => {
                eprintln!("mutsu-lsp: unrecognized argument {other:?}\n\n{USAGE}");
                return ExitCode::FAILURE;
            }
        }
    }

    let (connection, io_threads) = Connection::stdio();
    let result = mutsu_lsp::server::run(connection);
    // Join before reporting: the writer thread must finish flushing whatever
    // the loop queued, including the response to `shutdown`.
    let joined = io_threads.join();

    if let Err(e) = result {
        eprintln!("mutsu-lsp: {e}");
        return ExitCode::FAILURE;
    }
    if let Err(e) = joined {
        eprintln!("mutsu-lsp: transport: {e}");
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}
