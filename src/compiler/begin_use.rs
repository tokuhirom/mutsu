//! BEGIN-time module preloads for a `use` written inside a nested block.
//!
//! Raku performs `use Foo` at BEGIN time: by the time any of the compunit's
//! mainline runs, every package `Foo` installs is already there. mutsu compiles
//! `use` to the runtime [`OpCode::UseModule`], so a `use` inside a block that
//! has not executed yet is invisible to code that *runs* earlier — even when the
//! `use` appears earlier in file order:
//!
//! ```raku
//! my &later = { use UseTypeFixture; };   # earlier in the file, not run yet
//! say X::Fixture::Marker.^name;          # raku: UseTypeFixture::X::Fixture::Marker
//! ```
//!
//! The gap is filled here without moving the *import*, which Raku genuinely does
//! scope to the block holding the `use`. The unit compile records every module
//! `use`d anywhere inside it ([`UnitUseCtx`]); whatever is left after subtracting
//! the unit's own top-level `use`s is a nested one, and gets a
//! [`OpCode::PreloadModule`] at the head of the unit. That op performs the load
//! half only (`Interpreter::preload_module`), so the module's packages and types
//! are reachable from the whole mainline while its exports still enter scope
//! exactly where the `use` stands.
//!
//! Two details make the prologue safe to run that early:
//!
//! - The unit's literal `use lib` specs are replayed ahead of the preloads, in
//!   source order, because a `use lib` written *later* in the file is also a
//!   BEGIN-time effect that a hoisted load may depend on. Adding a path already
//!   in the chain is a no-op, so the `use lib` at its own position still behaves
//!   as before.
//! - A preload that cannot find its module is silently discarded. Nothing in the
//!   source asked for the module *here*, so a never-run block naming an absent
//!   module must stay as non-fatal as it is today; the in-position `UseModule`
//!   still reports the failure if control reaches it.
//!
//! See GH-8201.

use super::Compiler;
use crate::ast::{Expr, Stmt};
use crate::opcode::OpCode;
use crate::value::{Value, ValueView};
use std::sync::Mutex;

/// Per-compilation-unit record of the modules `use`d anywhere inside it,
/// including inside nested blocks and sub bodies. Shared with every child
/// compiler of the unit through `Compiler::inherit_fold_ctx`, which is why a
/// `use` compiled into a closure body still reaches the unit-level pass.
#[derive(Default)]
pub(crate) struct UnitUseCtx {
    modules: Mutex<Vec<String>>,
}

impl UnitUseCtx {
    /// Record a module `use`d while compiling this unit. Called from the general
    /// `Stmt::Use` arm, so it sees exactly the `use`s that compile to a real
    /// [`OpCode::UseModule`] load.
    pub(crate) fn note_use(&self, module: &str) {
        if let Ok(mut modules) = self.modules.lock() {
            modules.push(module.to_string());
        }
    }

    /// The modules this unit `use`s from somewhere other than its own top level
    /// — the ones whose load has to be hoisted. Computed as a multiset
    /// difference so a module `use`d both at the top level and inside a block
    /// still contributes the nested occurrence.
    pub(crate) fn nested_modules(&self, stmts: &[Stmt]) -> Vec<String> {
        let Ok(modules) = self.modules.lock() else {
            return Vec::new();
        };
        if modules.is_empty() {
            return Vec::new();
        }
        let mut top_level: Vec<&str> = stmts.iter().filter_map(preloadable_module).collect();
        let mut nested = Vec::new();
        for module in modules.iter() {
            if let Some(pos) = top_level.iter().position(|m| *m == module) {
                top_level.swap_remove(pos);
                continue;
            }
            if !nested.contains(module) {
                nested.push(module.clone());
            }
        }
        nested
    }
}

/// The module name a `use` statement contributes to the BEGIN-time preload set,
/// or `None` when it is not a plain module load.
///
/// Mirrors the filter the general `Stmt::Use` compile arm applies, so the
/// top-level subtraction in [`UnitUseCtx::nested_modules`] lines up with what
/// was recorded. Excluded: pragmas (which install no packages), `Test` and its
/// submodules (compiled by their own arm), a `:from<...>` foreign-language
/// compunit, a `use Foo:if(EXPR)` whose load is deliberately conditional on a
/// runtime value, and a `use Foo <args>` whose arguments feed the module's
/// `sub EXPORT` — that argument list is evaluated at the `use`'s own position,
/// so hoisting the load ahead of it would run `EXPORT` against the wrong input.
pub(crate) fn preloadable_module(stmt: &Stmt) -> Option<&str> {
    match stmt {
        Stmt::Use {
            module,
            tags,
            condition,
            arg,
        } => preloadable_module_name(module, tags, condition.as_deref(), arg.as_ref()),
        _ => None,
    }
}

pub(crate) fn preloadable_module_name<'a>(
    module: &'a str,
    tags: &[String],
    condition: Option<&Expr>,
    arg: Option<&Expr>,
) -> Option<&'a str> {
    if condition.is_some() || arg.is_some() {
        return None;
    }
    if tags.iter().any(|t| t == "from") {
        return None;
    }
    if is_pragma_like(module) || module == "Test" || module.starts_with("Test::") {
        return None;
    }
    Some(module)
}

/// A `use` of something that loads no compunit of its own. The same rule the
/// parser's module scan uses (`module_exports::import_is_pragma_like`): a
/// lowercase-initial name is a pragma, plus the handful of uppercase ones that
/// are built into the VM.
fn is_pragma_like(module: &str) -> bool {
    if module.starts_with(|c: char| !c.is_ascii_uppercase()) {
        return true;
    }
    matches!(
        module,
        "MONKEY" | "MONKEY-SEE-NO-EVAL" | "MONKEY-TYPING" | "MONKEY-GUTS" | "NativeCall"
    )
}

/// The literal repository specs the unit's `use lib` statements add, in source
/// order. Only literal spellings are collected: an expression form
/// (`use lib $dir`) cannot be replayed ahead of the mainline that computes it,
/// and a preload that then fails to resolve is discarded anyway.
pub(crate) fn literal_lib_paths(stmts: &[Stmt]) -> Vec<String> {
    let mut out = Vec::new();
    collect_lib_paths(stmts, &mut out);
    out
}

fn collect_lib_paths(stmts: &[Stmt], out: &mut Vec<String>) {
    for stmt in stmts {
        match stmt {
            Stmt::Use {
                module,
                arg: Some(arg),
                ..
            } if module == "lib" => push_literals(arg, out),
            Stmt::Block(body) | Stmt::SyntheticBlock(body) | Stmt::Package { body, .. } => {
                collect_lib_paths(body, out);
            }
            _ => {}
        }
    }
}

fn push_literals(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::Literal(v) => {
            if let ValueView::Str(s) = v.view() {
                let path = s.to_string();
                if !path.is_empty() && !out.contains(&path) {
                    out.push(path);
                }
            }
        }
        Expr::ArrayLiteral(items) => {
            for item in items {
                push_literals(item, out);
            }
        }
        Expr::Grouped(inner) => push_literals(inner, out),
        _ => {}
    }
}

impl Compiler {
    /// Emit the unit's BEGIN-time prologue: the replayed `use lib` specs, then
    /// one [`OpCode::PreloadModule`] per nested `use`. Both lists are empty
    /// unless the unit's first compile pass found a nested `use`, so a unit
    /// without one emits nothing at all.
    pub(super) fn emit_begin_preloads(&mut self) {
        if self.begin_preloads.is_empty() {
            return;
        }
        for path in std::mem::take(&mut self.begin_preload_lib_paths) {
            let idx = self.code.add_constant(Value::str(path));
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::UseLibPath);
        }
        for module in std::mem::take(&mut self.begin_preloads) {
            let idx = self.code.add_constant(Value::str(module));
            self.code.emit(OpCode::PreloadModule(idx));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn use_stmt(module: &str) -> Stmt {
        Stmt::Use {
            module: module.to_string(),
            arg: None,
            tags: Vec::new(),
            condition: None,
        }
    }

    #[test]
    fn pragmas_and_test_are_not_preloaded() {
        for name in ["v6", "lib", "strict", "MONKEY-TYPING", "NativeCall", "Test"] {
            assert!(
                preloadable_module(&use_stmt(name)).is_none(),
                "{name} should not be preloaded"
            );
        }
        assert_eq!(
            preloadable_module(&use_stmt("JSON::Tiny")),
            Some("JSON::Tiny")
        );
    }

    #[test]
    fn only_the_nested_use_is_hoisted() {
        let ctx = UnitUseCtx::default();
        ctx.note_use("Top::Level");
        ctx.note_use("Nested::One");
        let stmts = vec![use_stmt("Top::Level")];
        assert_eq!(ctx.nested_modules(&stmts), vec!["Nested::One".to_string()]);
    }

    #[test]
    fn a_module_used_both_places_still_hoists_the_nested_copy() {
        let ctx = UnitUseCtx::default();
        ctx.note_use("Both");
        ctx.note_use("Both");
        let stmts = vec![use_stmt("Both")];
        assert_eq!(ctx.nested_modules(&stmts), vec!["Both".to_string()]);
    }
}
