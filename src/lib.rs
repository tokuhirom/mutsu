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

/// Parse and compile source code, returning a disassembly of the compiled
/// bytecode: the mainline followed by each compiled function. Used for
/// compiler/JIT debugging (`--dump-bytecode`).
pub fn dump_bytecode(input: &str) -> Result<String, RuntimeError> {
    use std::fmt::Write;
    let (stmts, _) = parse_dispatch::parse_source(input)?;
    let mut compiler = compiler::Compiler::new();
    compiler.is_mainline = true;
    let (code, compiled_fns) = compiler.compile(&stmts);
    let mut out = String::new();
    let disasm = |out: &mut String, code: &opcode::CompiledCode| {
        for (i, op) in code.ops.iter().enumerate() {
            let _ = writeln!(out, "  {:4}: {:?}", i, op);
        }
        if !code.constants.is_empty() {
            let _ = writeln!(out, "  constants:");
            for (i, c) in code.constants.iter().enumerate() {
                let _ = writeln!(out, "    {:4}: {:?}", i, c);
            }
        }
        if !code.locals.is_empty() {
            let _ = writeln!(out, "  locals: {:?}", code.locals);
        }
    };
    let _ = writeln!(out, "== mainline ==");
    disasm(&mut out, &code);
    let mut names: Vec<&String> = compiled_fns.keys().collect();
    names.sort();
    for name in names {
        let cf = &compiled_fns[name];
        let _ = writeln!(out, "== sub {} ==", name);
        disasm(&mut out, &cf.code);
    }
    Ok(out)
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
