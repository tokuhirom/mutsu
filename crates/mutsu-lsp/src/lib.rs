//! The mutsu language server (ADR-0065).
//!
//! A language server *for mutsu*: it reports what mutsu makes of a document,
//! and its intended consumer is an AI agent writing Raku that will run on
//! mutsu. That consumer shapes every decision in here — see
//! `docs/adr/0065-language-server-targets-ai-agents.md`. The two that show up
//! most in this code are D3 (only the methods an agent consumes are
//! implemented, which removes incremental sync, completion and semantic tokens)
//! and D5 (the message matters more than the range, and the range must be
//! pinned by tests because an agent will never report that it is wrong).

pub mod diagnostics;
pub mod documents;
pub mod hover;
pub mod positions;
pub mod server;
pub mod symbols;
pub mod workspace;

/// The stack an analysis thread gets.
///
/// mutsu's parser is deeply recursive — grammar matching and nested expression
/// parsing each consume a sizeable native frame — which is why the interpreter's
/// own CLI (`src/main.rs`) does not run on the OS main thread either: it spawns
/// a 256 MB-stack thread and runs everything there.
///
/// The server must do the same, and this is not a nicety. A stack overflow
/// **aborts the process**; `catch_unwind` cannot turn it into a diagnostic the
/// way `mutsu::analysis::check` turns a panic into one. A resident server that
/// parses on a default 8 MB stack would die outright on a document the CLI reads
/// without complaint, taking every other open document with it.
pub const ANALYSIS_STACK_SIZE: usize = 256 * 1024 * 1024;

/// Run `f` on a thread with [`ANALYSIS_STACK_SIZE`], propagating a panic.
///
/// The binary runs its whole protocol loop inside this, so every parse the
/// session performs is on that stack; tests that analyse deep documents use it
/// for the same reason.
pub fn on_analysis_stack<F, R>(f: F) -> R
where
    F: FnOnce() -> R + Send + 'static,
    R: Send + 'static,
{
    let handle = std::thread::Builder::new()
        .name("mutsu-lsp-analysis".to_string())
        .stack_size(ANALYSIS_STACK_SIZE)
        .spawn(f)
        .expect("failed to spawn the analysis thread");
    match handle.join() {
        Ok(value) => value,
        Err(payload) => std::panic::resume_unwind(payload),
    }
}
