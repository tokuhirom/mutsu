//! The source text of the compilation unit being compiled, for `$*CU`.
//!
//! Rakudo's `$*CU` is the `RakuAST::CompUnit` of the unit under compilation,
//! visible to BEGIN blocks and to what `use` runs at compile time (an `EXPORT`
//! sub reaches the *importing* unit through it); at run time it is a Failure.
//! mutsu has no persistent AST of the in-flight unit, so the unit publishes its
//! source text here for its compile phase and `$*CU` is built from it on first
//! read, once per unit, with the same conversion `Str.AST` uses. Shaped like
//! [`crate::unit_source_file`]: a thread-local stack, a guard per unit.

use crate::value::{Value, ValueView};
use std::cell::RefCell;
use std::rc::Rc;

pub(crate) struct Unit {
    pub(crate) source: Rc<str>,
    /// The tree of a unit that is an EVAL'd RakuAST (it has no source text):
    /// `$*CU` is this tree wrapped as a `CompUnit`.
    tree: Option<Value>,
    /// The unit's compile phase is over (its BEGIN prologue has run).
    pub(crate) compiled: bool,
    cu: Option<Value>,
    /// Closures `$*CU.add-check-code` queued, each with the `CompUnit` it was
    /// called on, for the unit's CHECK phase.
    check_code: Vec<(Value, Value)>,
    /// The raw list `nqp::getattr($cu, RakuAST::CompUnit, '$!check-phasers')`
    /// answers: closures pushed onto it run in the unit's CHECK phase, like
    /// `add-check-code` ones. Created on first read.
    check_phasers: Option<Value>,
    /// Exceptions `$*CU.add-sorry` / `add-worry` queued, reported when the
    /// unit's compile phase ends (`unit_diagnostics`).
    pub(crate) sorries: Vec<Value>,
    pub(crate) worries: Vec<Value>,
    /// A module's `sub EXPORT` is running for this unit: `$*CU` is the
    /// importing unit below it, not this one.
    in_export: bool,
    /// Routines the unit imported after `$*CU` was built, which the tree's
    /// calls of them resolve to now (`publish_late_imports`).
    late_imports: std::collections::HashSet<String>,
}

thread_local! {
    pub(crate) static UNITS: RefCell<Vec<Unit>> = const { RefCell::new(Vec::new()) };
}

/// Publishes a unit's source for the lifetime of the guard.
pub(crate) struct UnitSourceTextGuard;

impl UnitSourceTextGuard {
    // Cost: O(n) to copy the source, n = source length.
    pub(crate) fn enter(source: &str) -> Self {
        UNITS.with(|units| {
            units.borrow_mut().push(Unit {
                source: Rc::from(source),
                tree: None,
                compiled: false,
                cu: None,
                check_code: Vec::new(),
                check_phasers: None,
                sorries: Vec::new(),
                worries: Vec::new(),
                in_export: false,
                late_imports: std::collections::HashSet::new(),
            });
        });
        Self
    }
}

impl UnitSourceTextGuard {
    /// Publishes an EVAL'd RakuAST tree as the unit being compiled.
    // Cost: O(1).
    pub(crate) fn enter_tree(tree: Value) -> Self {
        let guard = Self::enter("");
        UNITS.with(|units| {
            if let Some(unit) = units.borrow_mut().last_mut() {
                unit.tree = Some(tree);
            }
        });
        guard
    }
}

impl Drop for UnitSourceTextGuard {
    fn drop(&mut self) {
        UNITS.with(|units| {
            units.borrow_mut().pop();
        });
    }
}

/// The unit `$*CU` names: the innermost one not running a `sub EXPORT` of its
/// own (that one runs on behalf of the unit importing it).
pub(crate) fn active_index(units: &[Unit]) -> Option<usize> {
    units.iter().rposition(|unit| !unit.in_export)
}

/// While alive, the innermost unit is running its `sub EXPORT`, so `$*CU` and
/// `add-check-code` address the unit that imports it.
pub(crate) struct ExportPhaseGuard(Option<usize>);

impl ExportPhaseGuard {
    // Cost: O(1).
    pub(crate) fn enter() -> Self {
        UNITS.with(|units| {
            let mut units = units.borrow_mut();
            let index = units.len().checked_sub(1);
            if let Some(unit) = index.and_then(|i| units.get_mut(i)) {
                unit.in_export = true;
            }
            Self(index)
        })
    }
}

impl Drop for ExportPhaseGuard {
    fn drop(&mut self) {
        UNITS.with(|units| {
            let mut units = units.borrow_mut();
            if let Some(unit) = self.0.and_then(|i| units.get_mut(i)) {
                unit.in_export = false;
            }
        });
    }
}

/// `$cu.add-check-code($code)`: queue `code` on the innermost unit still in
/// its compile phase, to run (with no arguments) at its CHECK phase.
// Cost: O(1) amortized.
pub(crate) fn add_check_code(cu: Value, code: Value) {
    UNITS.with(|units| {
        let mut units = units.borrow_mut();
        let index = active_index(&units);
        // A unit that finished compiling never runs it, and rakudo does not
        // refuse the call either.
        if let Some(unit) = index
            .and_then(|i| units.get_mut(i))
            .filter(|unit| !unit.compiled)
        {
            unit.check_code.push((code, cu));
        }
    });
}

/// `$!check-phasers` of `$*CU`: the raw list of the innermost unit still in
/// its compile phase, whose closures run with the unit's check code.
/// `None` outside a compile phase.
// Cost: O(1) amortized.
pub(crate) fn check_phasers_list() -> Option<Value> {
    UNITS.with(|units| {
        let mut units = units.borrow_mut();
        let index = active_index(&units);
        let unit = index
            .and_then(|i| units.get_mut(i))
            .filter(|unit| !unit.compiled)?;
        Some(
            unit.check_phasers
                .get_or_insert_with(|| Value::vm_array(Vec::new()))
                .clone(),
        )
    })
}

/// The check code queued on the innermost unit, in the order it runs (last
/// queued first, as CHECK phasers do), then the closures pushed onto its
/// `$!check-phasers` list; both queues are emptied.
// Cost: O(c), c = queued closures.
pub(crate) fn take_check_code() -> Vec<(Value, Value)> {
    UNITS.with(|units| {
        let mut units = units.borrow_mut();
        let Some(unit) = units.last_mut() else {
            return Vec::new();
        };
        let mut queued = std::mem::take(&mut unit.check_code);
        queued.reverse();
        if let Some(list) = unit.check_phasers.take()
            && let ValueView::Array(data, _) = list.view()
        {
            let cu = unit.cu.clone().unwrap_or(Value::NIL);
            queued.extend(
                data.as_slice()
                    .iter()
                    .rev()
                    .map(|closure| (closure.clone(), cu.clone())),
            );
        }
        queued
    })
}

/// The innermost unit's compile phase is over: `$*CU` is a Failure from here on.
// Cost: O(1).
pub(crate) fn end_compile_phase() {
    UNITS.with(|units| {
        if let Some(unit) = units.borrow_mut().last_mut() {
            unit.compiled = true;
        }
    });
}

/// `$*CU`: the innermost unit still in its compile phase as a
/// `RakuAST::CompUnit`, built on first use. `None` outside a compile phase or
/// when the source does not convert.
// Cost: O(n) on the first read of a unit (parse + conversion), O(1) after;
// n = source length.
pub(crate) fn compile_time_cu(imported: &dyn Fn(&str) -> bool) -> Option<Value> {
    enum Step {
        Built(Value),
        Build(Rc<str>),
        BuildTree(Value),
    }
    let step = UNITS.with(|units| {
        let units = units.borrow();
        let unit = active_index(&units)
            .and_then(|i| units.get(i))
            .filter(|unit| !unit.compiled)?;
        Some(match (&unit.cu, &unit.tree) {
            (Some(cu), _) => Step::Built(cu.clone()),
            (None, Some(tree)) => Step::BuildTree(tree.clone()),
            (None, None) => Step::Build(unit.source.clone()),
        })
    })?;
    match step {
        Step::Built(cu) => Some(cu),
        Step::BuildTree(tree) => {
            let cu = crate::rakuast::compunit_of_tree(&tree, imported);
            UNITS.with(|units| {
                let mut units = units.borrow_mut();
                if let Some(unit) = active_index(&units).and_then(|i| units.get_mut(i)) {
                    unit.cu = Some(cu.clone());
                }
            });
            Some(cu)
        }
        Step::Build(source) => {
            let cu = crate::rakuast::compile_time_compunit(
                &crate::scope::NameScope::default(),
                &source,
                imported,
            )
            .ok()?;
            UNITS.with(|units| {
                let mut units = units.borrow_mut();
                if let Some(unit) = active_index(&units).and_then(|i| units.get_mut(i)) {
                    unit.cu = Some(cu.clone());
                }
            });
            Some(cu)
        }
    }
}

/// Before the unit's check code runs: record which calls of the trees the
/// queued closures were handed (`cus`) resolve to an imported routine by now.
/// Rakudo resolves in place before the CHECK phase; those trees are immutable,
/// so a call that found no import when its tree was built reads this set.
// Cost: O(n + u), n = nodes of the trees, u = their unresolved calls.
pub(crate) fn publish_late_imports<'a>(
    cus: impl Iterator<Item = &'a Value>,
    imported: &dyn Fn(&str) -> bool,
) {
    let mut names: Vec<String> = cus
        .flat_map(crate::rakuast::unresolved_call_names)
        .collect();
    names.sort();
    names.dedup();
    let late: std::collections::HashSet<String> =
        names.into_iter().filter(|name| imported(name)).collect();
    UNITS.with(|units| {
        let mut units = units.borrow_mut();
        if let Some(unit) = active_index(&units).and_then(|i| units.get_mut(i)) {
            unit.late_imports = late;
        }
    });
}

/// Whether the unit being compiled imported `name` after `$*CU` was built.
// Cost: O(1).
pub(crate) fn is_late_import(name: &str) -> bool {
    UNITS.with(|units| {
        let units = units.borrow();
        active_index(&units)
            .and_then(|i| units.get(i))
            .is_some_and(|unit| unit.late_imports.contains(name))
    })
}
