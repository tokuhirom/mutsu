//! The interpreter's side of a parse-time `BEGIN` (ADR-17558 slice 5): a
//! statement-form `BEGIN` of the program's top level runs when the parser
//! reaches it, in the interpreter that will run the program, interleaved with
//! the parse-time `use` loads in source order, as on rakudo.
//!
//! It runs there only when that is unobservable apart from the order: its body
//! reaches nothing of the unit, which has no frame yet
//! (`begin_prologue::begin_body_is_closed`). Any other `BEGIN` keeps its place
//! in the unit's BEGIN-time prologue (ADR-0134), and so does every compile-time
//! effect after it, a `use` included (ADR-17558 §7.1's transitional rule).

use super::*;
use crate::compile_time_loader::BeginOutcome;

thread_local! {
    /// Whether the innermost parse with a loader on this thread is the
    /// program's own (`run`, `-c`), rather than a module's compile, an `EVAL`
    /// or a `Str.AST`: only the program's unit-level `BEGIN`s run at parse
    /// time for now.
    static PROGRAM_PARSE: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Run `parse` with [`PROGRAM_PARSE`] set to `program`, restoring it after.
// Cost: O(1) besides `parse`.
pub(crate) fn with_parse_kind<T>(program: bool, parse: impl FnOnce() -> T) -> T {
    let saved = PROGRAM_PARSE.with(|flag| flag.replace(program));
    let result = parse();
    PROGRAM_PARSE.with(|flag| flag.set(saved));
    result
}

impl Interpreter {
    /// Run a unit-level statement-form `BEGIN` the parser just finished, if
    /// the parse is the program's and the body reaches nothing of the unit.
    // Cost: O(n) for the analysis, n = size of the phaser's body; the
    // phaser's own run otherwise.
    pub(crate) fn run_parse_time_begin(&mut self, begin: &Stmt) -> BeginOutcome {
        if !PROGRAM_PARSE.with(std::cell::Cell::get)
            || std::thread::current().name()
                == Some(crate::runtime::slang_activation::ACTIVATION_THREAD_NAME)
        {
            return BeginOutcome::NotTaken;
        }
        // The lowering and the run are not part of the unit's parse: a parse
        // they trigger (the lowering itself, a regex compiled at run time)
        // must not see or disturb the unit's parser state, which resumes
        // exactly where it was. Only the check of the names the body uses
        // against what the unit provides reads that state.
        let suspended = crate::parser::unit_state::suspend();
        // The parser's tree is lowered for the compiler as the whole unit's
        // is (`parse_compilation_unit_of`); the phaser alone needs nothing of
        // the rest of the unit to lower, since a closed body names none of it.
        let lowered = crate::rakuast::frontend::round_trip_unit(vec![begin.clone()]);
        let names = match lowered.as_deref() {
            Ok([Stmt::Phaser { body, .. }]) => {
                crate::runtime::begin_prologue::begin_body_core_names(body)
            }
            _ => None,
        };
        drop(suspended);
        let (Ok(lowered), Some(names)) = (lowered, names) else {
            return BeginOutcome::NotTaken;
        };
        if names.iter().any(|name| crate::parser::unit_provides_name(name)) {
            return BeginOutcome::NotTaken;
        }
        let suspended = crate::parser::unit_state::suspend();
        // Compiled as the prologue compiles it, so a failure is the same
        // `X::Comp::BeginTime` the prologue would raise.
        let mut compiler = crate::compiler::Compiler::new();
        compiler.set_current_package(self.current_package());
        compiler.is_mainline = true;
        let (code, compiled_fns) = compiler.compile(&lowered);
        let result = self.run_top(&code, &compiled_fns);
        drop(suspended);
        match result {
            Ok(_) => BeginOutcome::Ran,
            Err(err) => BeginOutcome::Failed(err),
        }
    }
}
