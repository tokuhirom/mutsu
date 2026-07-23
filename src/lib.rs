mod ast;
mod builtins;
mod compiler;
pub mod doc_mode;
pub(crate) mod env;
pub mod error_render;
pub(crate) mod gc;
mod interpreter;
mod opcode;
mod parse_dispatch;
mod parser;
pub(crate) mod precomp;
mod rakuast;
#[cfg(feature = "native")]
pub mod repl;
pub(crate) mod repl_core;
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
    let mut names: Vec<crate::symbol::Symbol> = compiled_fns.keys().copied().collect();
    names.sort_by_key(|s| s.resolve());
    for name in names {
        let cf = &compiled_fns[&name];
        let _ = writeln!(out, "== sub {} ==", name.as_str());
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

/// A stateful REPL session for the WASM site.
///
/// Unlike [`evaluate`], which spins up a fresh [`Interpreter`] per call, a
/// `Repl` keeps one interpreter alive so declarations persist across lines —
/// which is what the REPL page is built around. [`Repl::eval_block`] runs a
/// whole program against that same interpreter; the playground drives it
/// through a session it resets before every run, so each run is its own.
///
/// Line handling (bracket-continuation, last-value gist display) is delegated
/// to [`repl_core::process_line`], the same core the CLI REPL drives.
#[cfg(feature = "wasm")]
#[wasm_bindgen]
pub struct Repl {
    interpreter: Interpreter,
    accumulated: String,
}

#[cfg(feature = "wasm")]
#[wasm_bindgen]
impl Repl {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Repl {
        console_error_panic_hook::set_once();
        let mut interpreter = Interpreter::new();
        interpreter.set_program_path("<repl>");
        Repl {
            interpreter,
            accumulated: String::new(),
        }
    }

    /// Feed one line to the session.
    ///
    /// Returns a JSON object string: `{"output": String, "incomplete": bool}`.
    /// When `incomplete` is true the line was buffered (unbalanced brackets)
    /// and the caller should show a continuation prompt.
    #[wasm_bindgen(js_name = evalLine)]
    pub fn eval_line(&mut self, line: &str) -> String {
        let (result, display) =
            repl_core::process_line(&mut self.interpreter, &mut self.accumulated, line);
        let incomplete = matches!(result, repl_core::LineResult::Continue);
        json_result(&display.unwrap_or_default(), incomplete)
    }

    /// Run a whole script against the session's interpreter (the editor pane's
    /// Run button). Returns the same JSON shape as [`Repl::eval_line`], with
    /// `incomplete` always false.
    #[wasm_bindgen(js_name = evalBlock)]
    pub fn eval_block(&mut self, code: &str) -> String {
        self.accumulated.clear();
        self.interpreter.clear_output();
        let out = match self.interpreter.run(code) {
            Ok(_) => {
                let output = self.interpreter.output().to_string();
                self.interpreter.clear_output();
                self.interpreter.last_value.take();
                output
            }
            Err(err) => {
                let partial = self.interpreter.output().to_string();
                self.interpreter.clear_output();
                format!("{}Error: {}\n", partial, err.message)
            }
        };
        json_result(&out, false)
    }

    /// Drop all session state and start over with a fresh interpreter.
    pub fn reset(&mut self) {
        *self = Repl::new();
    }

    /// True when a previous line is still buffered awaiting its closing
    /// bracket (used to pick the `>` vs `*` prompt).
    #[wasm_bindgen(js_name = isPending)]
    pub fn is_pending(&self) -> bool {
        !self.accumulated.is_empty()
    }
}

#[cfg(feature = "wasm")]
impl Default for Repl {
    fn default() -> Self {
        Repl::new()
    }
}

/// Serialize a REPL step as `{"output": ..., "incomplete": ...}`. Hand-rolled
/// to keep the wasm bundle free of a serde derive for two fields.
#[cfg(feature = "wasm")]
fn json_result(output: &str, incomplete: bool) -> String {
    format!(
        "{{\"output\":{},\"incomplete\":{}}}",
        serde_json::Value::String(output.to_string()),
        incomplete
    )
}
