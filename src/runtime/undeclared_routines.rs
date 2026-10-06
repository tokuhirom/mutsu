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
//! nested block/class/sub suppresses the error even where raku's lexical
//! scoping would not), and the check bails out entirely when the unit imports
//! names it cannot see through (`use`/`need`/`import`/`require`). A missed
//! construct can only produce a false *negative* (the call is then caught at
//! runtime as before), never a false positive, as long as every construct the
//! call-walker descends into also has its declarations collected — both are
//! gathered in the same traversal to keep them symmetric.

use crate::ast::{CallArg, Expr, ParamDef, Stmt, UndeclaredRoutineCall};
use crate::ast_visit::{
    NameKind, Visit, walk_call_arg, walk_expr, walk_param, walk_stmt, walk_stmts,
};
use crate::value::{RuntimeError, RuntimeErrorCode};
use std::collections::HashSet;

use super::Interpreter;

mod conditional;

mod names;
use names::{COMPILER_SPECIAL_CALL_NAMES, NATIVE_TYPE_NAMES};
pub(crate) use names::{CORE_TERM_CONSTANTS, PHASER_SUGGESTION_NAMES};

#[derive(Default)]
struct Scan {
    declared: HashSet<String>,
    /// The subset of `declared` that is a *routine* declaration (`sub`, `multi
    /// sub`). `declared` deliberately also absorbs variables and types, because
    /// suppressing a call on any of them is the safe direction — but a
    /// suggestion drawn from it would propose a variable as the routine the
    /// user meant, which rakudo never does. Suggestions read this instead.
    declared_routines: HashSet<String>,
    calls: Vec<(String, i64)>,
    line: i64,
    /// The unit imports names the walker cannot see (use/require/...): skip
    /// the whole check rather than risk a false positive.
    bail: bool,
    /// The `EVAL` flavour of the check (`ScanMode::Eval`).
    eval: bool,
    /// The unit's top-level conditional `use`s (see [`conditional`]).
    conditional_uses: Vec<conditional::ConditionalUse>,
}

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

/// A declared name with its sigil and twigil removed.
fn bare_name(name: &str) -> &str {
    let bare = name.strip_prefix('\\').unwrap_or(name);
    bare.strip_prefix(['$', '@', '%', '&'])
        .unwrap_or(bare)
        .trim_start_matches(['!', '.', '*', '^', ':'])
}

impl Scan {
    /// Record a routine declaration: both a suppressor (like any other name)
    /// and a suggestion candidate.
    fn declare_routine(&mut self, name: &str) {
        self.declare(name);
        let bare = bare_name(name);
        if !bare.is_empty() {
            self.declared_routines.insert(bare.to_string());
        }
    }

    fn declare(&mut self, name: &str) {
        let bare = bare_name(name);
        if bare.is_empty() {
            return;
        }
        self.declared.insert(bare.to_string());
        // `sub term:<x> {...}` declares the bare term `x`.
        if let Some(inner) = bare
            .strip_prefix("term:<")
            .and_then(|s| s.strip_suffix('>'))
        {
            self.declared.insert(inner.to_string());
        }
    }

    fn record_call(&mut self, name: &str) {
        if self.bail {
            return;
        }
        // `require`/`import` in call form pull in names at runtime that the
        // walker cannot see — give up on the whole unit.
        if matches!(name, "require" | "import" | "need" | "use" | "EVALFILE") {
            self.bail = true;
            return;
        }
        let Some(first) = name.chars().next() else {
            return;
        };
        // Only plain lowercase bareword calls are checked: uppercase names are
        // type coercions (a different error class with many more legitimate
        // sources), qualified/adverbed names carry `:`/`::`, and `__`-names
        // are compiler-synthesized.
        if !(first.is_ascii_lowercase() || self.eval && first.is_ascii_uppercase())
            || name.contains(':')
            || name.starts_with("__")
        {
            return;
        }
        self.calls.push((name.to_string(), self.line));
    }
}

impl<'ast> Visit<'ast> for Scan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        // The `EVAL` scan keeps walking after a bail: its declarations also
        // feed the undeclared-name check (`scope_blind_declared_names`).
        if self.bail && !self.eval {
            return;
        }
        match stmt {
            Stmt::SetLine(n) => self.line = *n,
            Stmt::Use { module, .. } if !self.eval => {
                if !conditional::imports_no_routines(module) {
                    self.bail = true;
                }
            }
            Stmt::No { .. } | Stmt::Need { .. } | Stmt::Import { .. } if !self.eval => {
                self.bail = true;
            }
            // Dynamically-named sub: the declared name is unknowable.
            Stmt::SubDecl {
                name_expr: Some(_), ..
            } => self.bail = true,
            // A bare lowercase identifier standing alone as a whole statement
            // (`dead;`) is, syntactically, the exact same "no-args routine
            // reference" `record_call` already recognizes for `Stmt::Call` --
            // rakudo's parser resolves an unrecognized bare lowercase term this
            // way and reports the same "Undeclared routine" error for it
            // (verified against `raku`: a `unit class Foo; dead` body dies with
            // "Undeclared routine:\n    dead used at line N"), where mutsu
            // previously fell through the runtime's bareword resolution to a
            // plain Str. `self` is the one common legitimate bare statement
            // this parses to (a method's own bare `self` statement) and is
            // excluded explicitly.
            Stmt::Expr(Expr::BareWord(name)) if name != "self" => self.record_call(name),
            // An explicit-invocant call (`foo($obj: ...)`) is a method call.
            Stmt::Call { args, .. } if args.iter().any(|a| matches!(a, CallArg::Invocant(_))) => {
                for a in args {
                    walk_call_arg(self, a);
                }
            }
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        // A lowercase bareword in any expression position (`say bar`,
        // `my $x = bar`) is the same "no-args routine reference" as the
        // statement form above; rakudo rejects it at CHECK time too.
        if let Expr::BareWord(name) = expr
            && name != "self"
        {
            self.record_call(name);
        }
        walk_expr(self, expr);
    }

    fn visit_param(&mut self, param: &'ast ParamDef) {
        // A type capture (`::T`, `role R[::T]`, `method m(::T:)`) declares
        // the type name `T` for the rest of its scope.
        if let Some(capture) = &param.type_capture {
            self.declare(capture);
        }
        walk_param(self, param);
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        match kind {
            NameKind::Call | NameKind::UserRoutineCall => self.record_call(name),
            NameKind::SubDecl => self.declare_routine(name),
            // Declarations are collected scope-blind. Assignment targets are
            // not declarations, but suppressing calls to an assigned name is
            // the safe (false-negative) direction. A callable parameter
            // (`&f`, also a role's `role R[&f]`) is in scope as the routine
            // `f`.
            NameKind::Decl
            | NameKind::VarDecl
            | NameKind::AssignTarget
            | NameKind::Param
            | NameKind::BlockParam
            // A label (`L: for ...`) is a term in its scope (`last L`,
            // `:label(L)`).
            | NameKind::Label => self.declare(name),
            _ => {}
        }
    }
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
    crate::parser::is_imported_function(name) && !if_imports.contains(name)
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
    let calls = scan
        .calls
        .into_iter()
        .filter(|(name, _)| !known_from_the_unit_alone(name, &scan.declared))
        .collect();
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
        }
    }
}

fn scan_unit(stmts: &[Stmt], mode: ScanMode) -> Scan {
    let mut scan = Scan {
        line: 1,
        eval: mode == ScanMode::Eval,
        ..Default::default()
    };
    if scan.eval {
        walk_stmts(&mut scan, stmts);
        return scan;
    }
    for stmt in stmts {
        match conditional::conditional_use(stmt) {
            Some(cu) => scan.conditional_uses.push(cu),
            None => scan.visit_stmt(stmt),
        }
    }
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
    let calls = scan
        .calls
        .into_iter()
        .filter(|(name, _)| !known_without_an_interpreter(name, &scan.declared, &if_imports))
        .collect();
    Some(Unexplained {
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
/// the same set the undeclared-routine check suppresses with. Reused by the
/// `EVAL` undeclared-name check so the two agree on what "declared" means.
// Cost: O(n), n = size of the unit's AST.
pub(crate) fn scope_blind_declared_names(stmts: &[Stmt]) -> HashSet<String> {
    scan_unit(stmts, ScanMode::Eval).declared
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

    /// The undeclared-routine check for a unit of kind `mode`; see
    /// [`ScanMode`] for how an `EVAL`'d snippet differs.
    // Cost: O(n + c * r), n = size of the unit's AST, c = unexplained calls,
    // r = cost of one registry/env lookup.
    pub(crate) fn check_undeclared_routines(
        &self,
        stmts: &[Stmt],
        mode: ScanMode,
    ) -> Result<(), RuntimeError> {
        self.judge_unexplained_calls(stmts, mode, &mut Vec::new())
    }

    // Cost: O(n + c * r), as `check_undeclared_routines`.
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
            if self.has_function(name)
                || self.has_multi_function_unindexed(name)
                || self.has_proto(name)
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
