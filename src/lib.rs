mod ast;
mod builtins;
mod compiler;
pub mod doc_mode;
pub(crate) mod env;
pub(crate) mod gc;
mod interpreter;
mod opcode;
mod parse_dispatch;
mod parser;
pub(crate) mod precomp;
#[cfg(feature = "native")]
pub mod repl;
mod runtime;
pub mod symbol;
mod token_kind;
mod trace;
mod value;
mod vm;

pub use interpreter::Interpreter;
pub use value::{RuntimeError, RuntimeErrorCode, Value};

/// Print VM -> interpreter fallback statistics to stderr.
///
/// No-op unless the `MUTSU_VM_STATS` environment variable is set. Used to track
/// progress on decoupling the bytecode VM from the tree-walking interpreter.
pub fn dump_vm_stats() {
    vm::vm_stats::dump();
}

/// Register the calling thread as the interpreter's main mutator thread for
/// the GC's cooperative stop-the-world accounting (`gc::stw`). The CLI entry
/// point calls this once on its big-stack main thread; its quiescence
/// (blocked in `await`/`.finish`/`sleep`) then counts toward — and is
/// required by — a collector's rendezvous, exactly like a `spawn_user_thread`
/// worker's. Embedders that drive `Interpreter` from a long-lived thread and
/// enable `MUTSU_GC` should call this on that thread too.
pub fn gc_register_main_thread() {
    gc::enter_mutator_worker();
    gc::mark_thread_registered(true);
}

/// Parse source code and return a pretty-printed AST string.
pub fn dump_ast(input: &str) -> Result<String, RuntimeError> {
    let (stmts, _) = parse_dispatch::parse_source(input)?;
    Ok(format!("{:#?}", stmts))
}

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

/// Evaluate Raku code and return the output as a string.
/// This is the main entry point for the WASM build.
#[cfg(feature = "wasm")]
#[wasm_bindgen]
pub fn evaluate(code: &str) -> String {
    console_error_panic_hook::set_once();
    let mut interpreter = Interpreter::new();
    match interpreter.run(code) {
        Ok(output) => output,
        Err(err) => format!("Error: {}", err.message),
    }
}
