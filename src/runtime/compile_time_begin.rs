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

/// Whether the parse in progress is the program's own unit, on the program
/// interpreter's thread (not the slang activation interpreter's).
// Cost: O(1).
pub(crate) fn is_program_parse() -> bool {
    PROGRAM_PARSE.with(std::cell::Cell::get)
        && std::thread::current().name()
            != Some(crate::runtime::slang_activation::ACTIVATION_THREAD_NAME)
}

impl Interpreter {
    /// Run a unit-level statement-form `BEGIN` the parser just finished, if
    /// the parse is the program's and the body reaches nothing of the unit
    /// but core names, the routines the unit's parse-time `use`s import and
    /// its plain top-level `my` variables (`unit`). The imports it needs run
    /// first, as the prologue would have run them ahead of it; the variables
    /// are parse-time copies, whose values it leaves are handed back for the
    /// prologue to write into the unit's own (ADR-17558 §7.17).
    // Cost: O(n + u * i + v) for the analysis, n = size of the phaser's body,
    // u = `uses`, i = names each imports, v = the unit's variables; the
    // phaser's own run otherwise.
    pub(crate) fn run_parse_time_begin(
        &mut self,
        begin: &Stmt,
        unit: &crate::compile_time_loader::UnitAtParse<'_>,
    ) -> BeginOutcome {
        if !is_program_parse() {
            return BeginOutcome::NotTaken;
        }
        let uses = unit.uses;
        let imports_name = |use_stmt: &Stmt, name: &str| {
            matches!(use_stmt, Stmt::Use { imports, .. } if imports.iter().any(|i| i == name))
        };
        let imported = |name: &str| uses.iter().any(|u| imports_name(u, name));
        let unit_var = |name: &str| unit.vars.iter().any(|(var, _)| var == name);
        let reach = crate::runtime::begin_prologue::UnitReach {
            imported: &imported,
            unit_var: &unit_var,
        };
        // The lowering and the run are not part of the unit's parse: a parse
        // they trigger (the lowering itself, a regex compiled at run time)
        // must not see or disturb the unit's parser state, which resumes
        // exactly where it was. Only the check of the names the body uses
        // against what the unit provides reads that state.
        let suspended = crate::parser::unit_state::suspend();
        // The parser's tree is lowered for the compiler as the whole unit's
        // is (`parse_compilation_unit_of`); the phaser alone needs nothing of
        // the rest of the unit to lower, since a closed body names none of it.
        let lowered = crate::rakuast::frontend::finish_unit(vec![begin.clone()]);
        let closed = match lowered.as_deref() {
            Ok([Stmt::Phaser { body, bare, .. }]) => {
                crate::runtime::begin_prologue::begin_body_core_names(body, *bare, &reach)
            }
            _ => None,
        };
        drop(suspended);
        let (Ok(lowered), Some(closed)) = (lowered, closed) else {
            return BeginOutcome::NotTaken;
        };
        if closed.core.iter().any(|name| crate::parser::unit_provides_name(name))
            || closed.imported.iter().any(|name| crate::parser::unit_declares_name(name))
        {
            return BeginOutcome::NotTaken;
        }
        // The imports the body needs, in source order, ahead of it: the `use`
        // statements themselves, whose module is loaded, so running one only
        // imports (a hook module's map is the one its parse-time load
        // recorded for that statement's site).
        let mut program: Vec<Stmt> = uses
            .iter()
            .filter(|u| closed.imported.iter().any(|name| imports_name(u, name)))
            .cloned()
            .collect();
        let suspended = crate::parser::unit_state::suspend();
        if !program.is_empty() {
            match crate::rakuast::frontend::finish_unit(program) {
                Ok(lowered_uses) => program = lowered_uses,
                Err(_) => {
                    drop(suspended);
                    return BeginOutcome::NotTaken;
                }
            }
        }
        program.extend(lowered);
        // Compiled as the prologue compiles it, so a failure is the same
        // `X::Comp::BeginTime` the prologue would raise.
        let mut compiler = crate::compiler::Compiler::new();
        compiler.set_current_package(self.current_package());
        compiler.is_mainline = true;
        let (code, compiled_fns) = compiler.compile(&program);
        // The unit's variables it reaches are its free variables, which the
        // run resolves by name: bind each to its parse-time copy, starting
        // from what an earlier parse-time `BEGIN` left in it.
        let saved: Vec<(String, Option<Value>)> = closed
            .unit_vars
            .iter()
            .map(|name| {
                let start = unit
                    .vars
                    .iter()
                    .find(|(var, _)| var == name)
                    .and_then(|(_, value)| value.clone())
                    .unwrap_or_else(|| Self::parse_time_var_start(name));
                (name.clone(), self.env.insert(name.clone(), start))
            })
            .collect();
        let result = self.run_top(&code, &compiled_fns);
        let mut left = Vec::with_capacity(saved.len());
        for (name, previous) in saved {
            let value = match previous {
                Some(previous) => self.env.insert(name.clone(), previous),
                None => self.env.remove(&name),
            };
            left.push((name, value.unwrap_or(Value::NIL)));
        }
        drop(suspended);
        // An `exit` in it ends the process here, before the rest of the unit
        // is parsed (rakudo: `BEGIN { plan 1; skip-rest; exit }; 1 +;` reports
        // no syntax error). The parse stops with this error, which the
        // program's run does not report: it sees the exit and finishes.
        if self.control.halted {
            return BeginOutcome::Failed(RuntimeError::new("exit in a parse-time BEGIN"));
        }
        match result {
            Ok(_) => BeginOutcome::Ran(left),
            Err(err) => BeginOutcome::Failed(err),
        }
    }

    /// The value a plain `my` variable named `name` (the compiler's name:
    /// `x`, `@a`, `%h`) holds before anything is assigned to it.
    // Cost: O(1).
    fn parse_time_var_start(name: &str) -> Value {
        match name.as_bytes().first() {
            Some(b'@') => Value::real_array(Vec::new()),
            Some(b'%') => Value::hash(crate::value::HashData::default()),
            _ => Value::package(crate::symbol::Symbol::intern("Any")),
        }
    }
}
