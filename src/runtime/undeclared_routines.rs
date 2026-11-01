//! Compile-time detection of calls to undeclared routines in the mainline.
//!
//! Rakudo resolves routine names at CHECK time (end of compilation), so a call
//! to a routine that is declared nowhere in the compilation unit aborts before
//! anything runs: `say 42; nosuchsub()` prints nothing and exits with
//! `===SORRY!=== ... Undeclared routine:\n    nosuchsub used at line 1`.
//!
//! The walk is the typed AST visitor (ADR-0137). It is deliberately
//! conservative in the safe direction: declarations
//! are collected *scope-blind* from the whole unit (a sub declared inside any
//! nested block or class suppresses the error even where raku's lexical
//! scoping would not), with one exception: a routine declared inside another
//! routine's body is visible only to the calls inside that body (#13111; see
//! [`scan`]). The check bails out entirely when the unit imports
//! names it cannot see through (`use`/`need`/`import`/`require`). A missed
//! construct can only produce a false *negative* (the call is then caught at
//! runtime as before), never a false positive, as long as every construct the
//! call-walker descends into also has its declarations collected — both are
//! gathered in the same traversal to keep them symmetric.

use crate::ast::{Stmt, UndeclaredRoutineCall};
use crate::value::{RuntimeError, RuntimeErrorCode};
use std::collections::HashSet;

use super::Interpreter;

mod conditional;

mod names;
use names::{COMPILER_SPECIAL_CALL_NAMES, NATIVE_TYPE_NAMES};
pub(crate) use names::{CORE_TERM_CONSTANTS, PHASER_SUGGESTION_NAMES};

mod scan;
use scan::Scan;

/// Which compilation unit the scan judges.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum ScanMode {
    /// A program, module or `require`d file.
    Mainline,
    /// An `EVAL`'d snippet. Its `use` statements do not end the check: the
    /// parser has already harvested the names they import
    /// (`parser::is_imported_function`), and the caller's own routines are in
    /// scope, so a still-unknown name is reported as rakudo does
    /// (`EVAL 'use Test; zork()'`). A capitalised callee (`Zork(1)`) is judged
    /// too: rakudo rejects an undeclared one at compile time
    /// ("Undeclared name"), and a type of that name suppresses it.
    Eval,
}

/// Whether a `use` of `module` imports no names at all (a pragma, `lib`).
pub(crate) fn module_imports_no_names(module: &str) -> bool {
    conditional::imports_no_routines(module)
}

/// Whether `name` is a routine mutsu knows about from *static* tables alone —
/// the compilation unit's own declarations, the built-in and Test routine
/// name lists, the native type-name coercions, the compiler's special call
/// names, and whatever the parser harvested from this unit's imports.
///
/// Split out so the analysis frontend (`crate::analysis`, ADR-0065) can reach
/// the same verdict without an `Interpreter`. Everything the *runtime* entry
/// point additionally consults is per-interpreter registry state, which is
/// empty in a freshly constructed one — so the two paths agree on any unit the
/// frontend sees, and the one list of static predicates lives here rather than
/// being duplicated and left to drift.
///
/// `if_imports` are the names only a conditional `use` imported: they do not
/// explain a call (see [`conditional`]).
fn known_without_an_interpreter(
    name: &str,
    declared: &HashSet<String>,
    if_imports: &HashSet<&str>,
) -> bool {
    known_from_the_unit_alone(name, declared) || imported_here(name, if_imports)
}

/// The part of [`known_without_an_interpreter`] that depends on the unit's
/// AST alone, so it can be recorded with the unit (ADR-12026 §2.1).
fn known_from_the_unit_alone(name: &str, declared: &HashSet<String>) -> bool {
    declared.contains(name)
        || Interpreter::is_builtin_function(name)
        || Interpreter::is_test_function_name(name)
        || super::system_eval_names::EVAL_KNOWN_ROUTINE_NAMES.contains(&name)
        || NATIVE_TYPE_NAMES.contains(&name)
        || COMPILER_SPECIAL_CALL_NAMES.contains(&name)
}

/// The part of [`known_without_an_interpreter`] that asks the parser's
/// import table, which is state at the moment of the check.
fn imported_here<S: std::borrow::Borrow<str> + Eq + std::hash::Hash>(
    name: &str,
    if_imports: &HashSet<S>,
) -> bool {
    // An exported sigilless `constant` (`my constant answer is export = 5`)
    // is a term, not a routine, but a bare use of it parses to the same
    // no-args call shape and is just as much explained by the import.
    (crate::parser::is_imported_function(name) || crate::parser::is_imported_value_term(name))
        && !if_imports.contains(name)
}

/// What the walker found: every call the static tables cannot explain, in
/// source order, plus the unit's own routine declarations for suggestions.
///
/// `None` means the unit must not be judged at all — it imports names the
/// walker cannot see through, so any verdict would risk a false positive.
struct Unexplained {
    calls: Vec<(String, i64)>,
    declared_routines: HashSet<String>,
    /// The slots holding the `:if` values of the unit's conditional `use`s.
    /// When there are any, a call is an error only if none of them loaded.
    condition_slots: Vec<String>,
    /// The unexplained mainline calls whose name the unit declares only
    /// inside routine bodies. An `our` one of those is already in the
    /// registry (#13091), but a bare call outside its body does not reach
    /// it (#13111), so the registry's routines do not explain these.
    body_scoped: HashSet<String>,
}

/// Whether a unit's imports cannot be seen before it runs: a `use` of a module
/// that may import routines or types, after a `use lib` that its module search
/// depends on. The unit's `use`s are pre-run only up to the first `use lib`
/// (`run_eval_hook_uses`), so whatever the later ones import is unknown to the
/// CHECK-time name checks, and judging any bareword would risk a false positive,
/// exactly as the mainline check gives up on a unit with such a `use`.
// Cost: O(s), s = statements of the unit.
pub(crate) fn imports_follow_use_lib(stmts: &[Stmt]) -> bool {
    let mut after_lib = false;
    for stmt in stmts {
        match stmt {
            Stmt::Use { module, .. } if module == "lib" => after_lib = true,
            Stmt::Use { module, .. } if after_lib && !conditional::imports_no_routines(module) => {
                return true;
            }
            _ => {}
        }
    }
    false
}

/// The AST half of the mainline undeclared-routine check, recorded with a
/// precompiled unit (ADR-12026 §2.1): every call that the unit's own
/// declarations and the static tables leave unexplained. The parser's import
/// table and the registry are asked again on each load
/// ([`Interpreter::guards_for_recorded_calls`]), exactly as an unrecorded
/// check asks them.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct RecordedCalls {
    calls: Vec<(String, i64)>,
    declared_routines: Vec<String>,
    condition_slots: Vec<String>,
    if_imports: Vec<String>,
    #[serde(default)]
    body_scoped: Vec<String>,
}

/// Record the mainline check's AST half for `stmts`, or `None` when the unit
/// is not judged at all.
// Cost: O(n), n = size of the unit's AST.
pub(crate) fn record_mainline_calls(stmts: &[Stmt]) -> Option<RecordedCalls> {
    let scan = scan_unit(stmts, ScanMode::Mainline);
    if scan.bail {
        return None;
    }
    let mut if_imports: Vec<String> = scan
        .conditional_uses
        .iter()
        .flat_map(|cu| cu.imports.iter().cloned())
        .collect();
    if_imports.sort();
    if_imports.dedup();
    let calls: Vec<(String, i64)> = scan
        .calls
        .into_iter()
        .filter(|(name, _)| !known_from_the_unit_alone(name, &scan.declared))
        .collect();
    let mut body_scoped: Vec<String> = body_scoped_calls(&calls, &scan.body_routines)
        .into_iter()
        .collect();
    body_scoped.sort();
    let mut declared_routines: Vec<String> = scan.declared_routines.into_iter().collect();
    declared_routines.sort();
    Some(RecordedCalls {
        calls,
        declared_routines,
        condition_slots: scan
            .conditional_uses
            .into_iter()
            .map(|cu| cu.slot)
            .collect(),
        if_imports,
        body_scoped,
    })
}

impl RecordedCalls {
    /// The [`Unexplained`] a fresh scan would find now: the recorded calls
    /// minus those the parser's import table explains at this moment.
    // Cost: O(c), c = recorded calls.
    fn unexplained_now(&self) -> Unexplained {
        let if_imports: HashSet<&str> = self.if_imports.iter().map(String::as_str).collect();
        Unexplained {
            calls: self
                .calls
                .iter()
                .filter(|(name, _)| !imported_here(name, &if_imports))
                .cloned()
                .collect(),
            declared_routines: self.declared_routines.iter().cloned().collect(),
            condition_slots: self.condition_slots.clone(),
            body_scoped: self.body_scoped.iter().cloned().collect(),
        }
    }
}

/// The names of `calls` that the unit declares only in routine bodies.
// Cost: O(c), c = calls.
fn body_scoped_calls(calls: &[(String, i64)], body_routines: &HashSet<String>) -> HashSet<String> {
    calls
        .iter()
        .filter(|(name, _)| body_routines.contains(name))
        .map(|(name, _)| name.clone())
        .collect()
}

fn scan_unit(stmts: &[Stmt], mode: ScanMode) -> Scan {
    let mut scan = Scan::new(mode == ScanMode::Eval);
    if scan.eval {
        scan.walk_unit(stmts);
    } else {
        for stmt in stmts {
            match conditional::conditional_use(stmt) {
                Some(cu) => scan.conditional_uses.push(cu),
                None => scan.walk_top_level(stmt),
            }
        }
    }
    scan.resolve_body_scoped_calls();
    scan
}

// Cost: O(n), n = size of the unit's AST.
fn unexplained_calls(stmts: &[Stmt], mode: ScanMode) -> Option<Unexplained> {
    let scan = scan_unit(stmts, mode);
    if scan.bail {
        return None;
    }
    let if_imports: HashSet<&str> = scan
        .conditional_uses
        .iter()
        .flat_map(|cu| cu.imports.iter().map(String::as_str))
        .collect();
    let calls: Vec<(String, i64)> = scan
        .calls
        .into_iter()
        .filter(|(name, _)| !known_without_an_interpreter(name, &scan.declared, &if_imports))
        .collect();
    // An `EVAL`'d snippet's caller may have a routine of the same name, which
    // only the registry knows about.
    let body_scoped = if mode == ScanMode::Mainline {
        body_scoped_calls(&calls, &scan.body_routines)
    } else {
        HashSet::new()
    };
    Some(Unexplained {
        body_scoped,
        calls,
        declared_routines: scan.declared_routines,
        condition_slots: scan
            .conditional_uses
            .into_iter()
            .map(|cu| cu.slot)
            .collect(),
    })
}

/// Every name the unit declares anywhere — routines, variables (without
/// their sigil), parameters, types, enum keys, terms — collected scope-blind,
/// the same set the undeclared-routine check suppresses with (plus the
/// routines it scopes to their enclosing routine body). Reused by the
/// `EVAL` undeclared-name check so the two agree on what "declared" means.
// Cost: O(n), n = size of the unit's AST.
pub(crate) fn scope_blind_declared_names(stmts: &[Stmt]) -> HashSet<String> {
    let scan = scan_unit(stmts, ScanMode::Eval);
    let mut declared = scan.declared;
    declared.extend(scan.body_routines);
    declared
}

/// The CHECK-time undeclared-routine analysis, without constructing an
/// `Interpreter`.
///
/// This is what a language server calls (ADR-0065 S2). Running the ordinary
/// entry point against a *fresh* `Interpreter` would reach the same verdict —
/// its extra lookups are all registry state a new interpreter has none of — but
/// constructing one costs about 9 ms and retains roughly 7 KiB (measured on a
/// debug build, 2026-09-03, `tests/long_lived_parse.rs`), which a resident
/// process would pay on every keystroke. See
/// interpreter-new-is-expensive-and-retains-memory (#7572).
pub(crate) fn check_undeclared_routines_without_interpreter(
    stmts: &[Stmt],
) -> Result<(), RuntimeError> {
    let Some(found) = unexplained_calls(stmts, ScanMode::Mainline) else {
        return Ok(());
    };
    // A verdict that depends on a BEGIN-time `:if` value is not reported.
    if !found.condition_slots.is_empty() {
        return Ok(());
    }
    let Some((name, line)) = found.calls.first() else {
        return Ok(());
    };
    let suggestions = Interpreter::static_routine_suggestions(name, &found.declared_routines);
    Err(Interpreter::undeclared_routine_error(
        name,
        *line,
        suggestions,
    ))
}

impl Interpreter {
    /// Build the X::Undeclared::Symbols error for an undeclared routine call,
    /// rakudo-style: `Undeclared routine:\n    <name> used at line <N>` plus
    /// `. Did you mean '<s>'?` when there are suggestions. Marked as a
    /// compile-time error (ParseGeneric + line) so the CLI renders it as
    /// `===SORRY!=== Error while compiling ...`.
    pub(crate) fn undeclared_routine_error(
        name: &str,
        line: i64,
        suggestions: Vec<String>,
    ) -> RuntimeError {
        let mut msg = format!("Undeclared routine:\n    {} used at line {}", name, line);
        if !suggestions.is_empty() {
            msg.push_str(&format!(". Did you mean '{}'?", suggestions.join("', '")));
        }
        let mut err = RuntimeError::undeclared_routine_symbols(name, msg, suggestions);
        err.set_code(Some(RuntimeErrorCode::ParseGeneric));
        if line > 0 {
            err.set_line(Some(line as usize));
        }
        err
    }

    /// Reject a mainline call to a routine that is declared nowhere in the
    /// unit, *before* execution starts (rakudo's CHECK-time
    /// X::Undeclared::Symbols). See the module doc for the conservativeness
    /// contract; returns Ok(()) whenever the unit imports unseen names.
    ///
    /// A verdict that depends on a conditional `use` is dropped: a caller that
    /// runs the unit's BEGIN prologue uses
    /// [`Self::check_undeclared_routines_with_guards`] instead.
    pub(crate) fn check_undeclared_routines_mainline(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        self.check_undeclared_routines_with_guards(stmts)
            .map(|_| ())
    }

    /// The mainline check for a unit whose BEGIN prologue (ADR-0134) is in
    /// place. A call that only a conditional `use` could explain is returned
    /// as a guard statement, to be placed right after the prologue, which
    /// raises the error when none of the unit's conditional `use`s loaded
    /// (#10331).
    // Cost: O(n + c * r), n = size of the unit's AST, c = unexplained calls,
    // r = cost of one registry/env lookup.
    pub(crate) fn check_undeclared_routines_with_guards(
        &self,
        stmts: &[Stmt],
    ) -> Result<Vec<Stmt>, RuntimeError> {
        let mut guards = Vec::new();
        self.judge_unexplained_calls(stmts, ScanMode::Mainline, &mut guards)?;
        Ok(guards)
    }

    /// The undeclared-routine check for an `EVAL`'d unit (see [`ScanMode`]
    /// for how it differs from the mainline's), whose `use` statements have
    /// not run yet: a call `explained` accepts (a name the unit's `use`d
    /// modules declare, `use M :Tag; C`) is not judged.
    // Cost: O(n + c * r), n = size of the unit's AST, c = unexplained calls,
    // r = cost of one registry/env lookup.
    pub(crate) fn check_eval_unit_routines(
        &self,
        stmts: &[Stmt],
        explained: impl Fn(&str) -> bool,
    ) -> Result<(), RuntimeError> {
        let Some(mut found) = unexplained_calls(stmts, ScanMode::Eval) else {
            return Ok(());
        };
        found.calls.retain(|(name, _)| !explained(name));
        self.judge_found(&found, &mut Vec::new())
    }

    // Cost: O(n + c * r), as `check_eval_unit_routines`.
    fn judge_unexplained_calls(
        &self,
        stmts: &[Stmt],
        mode: ScanMode,
        guards: &mut Vec<Stmt>,
    ) -> Result<(), RuntimeError> {
        let Some(found) = unexplained_calls(stmts, mode) else {
            return Ok(());
        };
        self.judge_found(&found, guards)
    }

    /// [`Self::check_undeclared_routines_with_guards`] for a unit whose AST
    /// half was recorded ([`record_mainline_calls`]).
    // Cost: O(c * r), c = recorded calls, r = cost of one registry/env lookup.
    pub(crate) fn guards_for_recorded_calls(
        &self,
        recorded: Option<&RecordedCalls>,
    ) -> Result<Vec<Stmt>, RuntimeError> {
        let mut guards = Vec::new();
        if let Some(recorded) = recorded {
            self.judge_found(&recorded.unexplained_now(), &mut guards)?;
        }
        Ok(guards)
    }

    // Cost: O(c * r), as `guards_for_recorded_calls`.
    fn judge_found(&self, found: &Unexplained, guards: &mut Vec<Stmt>) -> Result<(), RuntimeError> {
        let mut guarded: HashSet<&str> = HashSet::new();
        for (name, line) in &found.calls {
            // Everything beyond the static tables is per-interpreter registry
            // state, which is why the analysis frontend can skip it entirely.
            let registered = !found.body_scoped.contains(name)
                && (self.has_function(name)
                    || self.has_multi_function_unindexed(name)
                    || self.has_proto(name));
            if registered
                // A routine an enclosing block's `use` bound lexically
                // (ADR-8751): an `EVAL` in that block calls it.
                || self.lexically_imported_routine(name)
                || self.env().contains_key(&format!("&{}", name))
                || self.env().contains_key(name.as_str())
                || self.get_our_var(name).is_some()
                // A sigilless constant in scope: an `EVAL` of a bare term
                // (`EVAL 'indiana-pi'` for a `--> indiana-pi` return value).
                || self.term_binding(name).is_some()
                // An enum key (`enum E <aa bb>; EVAL 'aa'`, #11818) -- a
                // module's own key only where that module is merged
                // (ADR-11136), like its classes.
                || (self.enum_bare_value(name).is_some() && !self.module_name_hidden_here(name))
                || self.registry().classes.contains_key(name)
                || self.registry().roles.contains_key(name)
                || self.registry().subsets.contains_key(name)
                || self.registry().enum_types.contains_key(name)
                // A capitalised callee (only judged in `EVAL`) is a coercion
                // when a type of that name exists, and a core term (`IterationEnd`)
                // is not a routine call at all.
                || (name.starts_with(|c: char| c.is_ascii_uppercase())
                    && (self.has_type(name)
                        || self.has_class(name)
                        || Self::is_builtin_type(name)
                        || super::eval_name_scans::is_core_term(name)))
            {
                continue;
            }
            // Rakudo suggests the unit's own routines, not just core ones:
            // `sub greeting {}; greetng()` answers "Did you mean 'greeting'?".
            // The interpreter's registry does not hold them at this point for
            // every declaration form, and the walker has already collected
            // them, so pass them along as extra candidates.
            let suggestions = self.suggest_routine_names_including(name, &found.declared_routines);
            if found.condition_slots.is_empty() {
                return Err(Self::undeclared_routine_error(name, *line, suggestions));
            }
            if !guarded.insert(name) {
                continue;
            }
            // Rakudo reports the first undeclared routine; so does the first
            // guard that fires.
            let call = UndeclaredRoutineCall {
                name: name.clone(),
                line: *line,
                suggestions,
            };
            guards.push(conditional::guard(call, &found.condition_slots));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::check_undeclared_routines_without_interpreter as check;

    fn parse(src: &str) -> Vec<crate::ast::Stmt> {
        crate::parser::parse_program(src).expect("parse").0
    }

    #[test]
    fn an_attribute_does_not_declare_a_routine() {
        assert!(check(&parse("class C { has $.foo; method m { foo() } }")).is_err());
        assert!(check(&parse("class C { has $.foo; method m { $.foo } }")).is_ok());
    }

    #[test]
    fn a_call_in_a_compound_assignment_is_found() {
        assert!(check(&parse("my $x = 1; $x += nosuch()")).is_err());
        assert!(check(&parse("sub there { 1 }; my $x = 1; $x += there()")).is_ok());
    }
}
