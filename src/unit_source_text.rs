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
    /// The unit is the lowered, CHECK-rewritten tree of a unit whose check
    /// closures already ran: its own (second) `use` lines queue the same
    /// closures again, which must not rewrite the tree twice.
    rewritten: bool,
    /// The fingerprint of `$*CU` as the unit's CHECK phase found it: taken
    /// when its BEGIN prologue ends, or when the tree is built after that. A
    /// tree that differs from it at the end of the CHECK phase was rewritten
    /// by a CHECK phaser or check closure (#17501).
    check_baseline: Option<u64>,
}

thread_local! {
    /// The next [`UnitSourceTextGuard::enter_tree`] is the restart of a unit
    /// whose CHECK phase rewrote `$*CU` ([`mark_next_tree_rewritten`]).
    static NEXT_TREE_REWRITTEN: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
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
                rewritten: false,
                check_baseline: None,
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
        let rewritten = NEXT_TREE_REWRITTEN.with(|flag| flag.replace(false));
        UNITS.with(|units| {
            if let Some(unit) = units.borrow_mut().last_mut() {
                unit.tree = Some(tree);
                unit.rewritten = rewritten;
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

/// While alive, the active unit counts as in its compile phase again. A
/// block-scoped `use` re-runs its module's `sub EXPORT` when the block runs
/// (`rerun_module_export`), after the unit's compile phase ended; in rakudo
/// that `use` runs at compile time, so the hook still sees `$*CU` (#15371).
pub(crate) struct ReopenCompilePhaseGuard(Option<(usize, bool)>);

impl ReopenCompilePhaseGuard {
    // Cost: O(u), u = units on the stack.
    pub(crate) fn enter() -> Self {
        UNITS.with(|units| {
            let mut units = units.borrow_mut();
            let index = active_index(&units);
            Self(index.and_then(|i| {
                let unit = units.get_mut(i)?;
                let was = unit.compiled;
                unit.compiled = false;
                Some((i, was))
            }))
        })
    }
}

impl Drop for ReopenCompilePhaseGuard {
    fn drop(&mut self) {
        UNITS.with(|units| {
            let mut units = units.borrow_mut();
            if let Some((i, was)) = self.0
                && let Some(unit) = units.get_mut(i)
            {
                unit.compiled = was;
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
        if unit.rewritten {
            unit.check_phasers = None;
            return Vec::new();
        }
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

/// Whether the unit compiling now is the restart of one whose CHECK phase
/// rewrote `$*CU`: its BEGIN and CHECK phasers ran in the first compile, so a
/// statement-form phaser body is skipped and its top-level `my` declarations
/// adopt the first compile's bindings (#17676/#17681).
// Cost: O(1).
pub(crate) fn active_unit_is_rewritten() -> bool {
    UNITS.with(|units| {
        let units = units.borrow();
        active_index(&units)
            .and_then(|i| units.get(i))
            .is_some_and(|unit| unit.rewritten && !unit.compiled)
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
pub(crate) fn compile_time_cu(
    imported: &dyn Fn(&str) -> bool,
    imported_type: &dyn Fn(&str) -> bool,
) -> Option<Value> {
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
            let cu = crate::rakuast::compunit_of_tree(&tree, imported, imported_type);
            let fingerprint = tree_fingerprint(&cu);
            UNITS.with(|units| {
                let mut units = units.borrow_mut();
                if let Some(unit) = active_index(&units).and_then(|i| units.get_mut(i)) {
                    unit.cu = Some(cu.clone());
                    unit.check_baseline = Some(fingerprint);
                }
            });
            Some(cu)
        }
        Step::Build(source) => {
            let cu = crate::rakuast::compile_time_compunit(
                &crate::scope::NameScope::default(),
                &source,
                imported,
                imported_type,
            )
            .ok()?;
            let fingerprint = tree_fingerprint(&cu);
            UNITS.with(|units| {
                let mut units = units.borrow_mut();
                if let Some(unit) = active_index(&units).and_then(|i| units.get_mut(i)) {
                    unit.cu = Some(cu.clone());
                    unit.check_baseline = Some(fingerprint);
                }
            });
            Some(cu)
        }
    }
}

/// `f` applied to the active unit's `$*CU`, when it was built; never builds it.
// Cost: O(1) plus `f`.
pub(crate) fn with_active_cu<T>(f: impl FnOnce(&Value) -> Option<T>) -> Option<T> {
    UNITS.with(|units| {
        let units = units.borrow();
        let unit = active_index(&units).and_then(|i| units.get(i))?;
        f(unit.cu.as_ref()?)
    })
}

/// Before the unit's check code runs: build `$*CU` again when a module read it
/// before the unit's own `use` lines and slang productions had run, so the
/// types they brought (a class Qwiratry's `aggsub` minted) resolve in the tree
/// the check code reads. The check code has not run yet, so nothing it could
/// have changed is lost; the queues live in the unit, not in the tree.
// Cost: O(n * d), n = nodes of the tree, d = scope depth; O(1) when `$*CU` was never read.
pub(crate) fn refresh_cu_resolution(
    imported: &dyn Fn(&str) -> bool,
    imported_type: &dyn Fn(&str) -> bool,
) {
    enum Source {
        Source(Rc<str>),
        Tree(Value),
    }
    let source = UNITS.with(|units| {
        let units = units.borrow();
        let unit = active_index(&units).and_then(|i| units.get(i))?;
        if unit.cu.is_none() || unit.rewritten {
            return None;
        }
        match &unit.tree {
            Some(tree) => Some(Source::Tree(tree.clone())),
            None => Some(Source::Source(unit.source.clone())),
        }
    });
    let cu = match source {
        None => return,
        Some(Source::Tree(tree)) => Some(crate::rakuast::compunit_of_tree(
            &tree,
            imported,
            imported_type,
        )),
        Some(Source::Source(source)) => crate::rakuast::compile_time_compunit(
            &crate::scope::NameScope::default(),
            &source,
            imported,
            imported_type,
        )
        .ok(),
    };
    if let Some(cu) = cu {
        let fingerprint = tree_fingerprint(&cu);
        UNITS.with(|units| {
            let mut units = units.borrow_mut();
            if let Some(unit) = active_index(&units).and_then(|i| units.get_mut(i)) {
                unit.cu = Some(cu);
                unit.check_baseline = Some(fingerprint);
            }
        });
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

/// The next EVAL'd tree is the restart of a unit whose CHECK phase rewrote its
/// `$*CU`: its check closures are not run again.
// Cost: O(1).
pub(crate) fn mark_next_tree_rewritten() {
    NEXT_TREE_REWRITTEN.with(|flag| flag.set(true));
}

/// The `$*CU` tree of the innermost unit when its CHECK phase changed it
/// (`check_baseline`): the tree the rest of the unit is to run from.
// Cost: O(n), n = nodes of the tree; O(1) when `$*CU` was never built.
pub(crate) fn rewritten_cu() -> Option<Value> {
    let (cu, baseline) = UNITS.with(|units| {
        let units = units.borrow();
        let unit = units.last()?;
        if unit.rewritten {
            return None;
        }
        Some((unit.cu.clone()?, unit.check_baseline?))
    })?;
    (tree_fingerprint(&cu) != baseline).then_some(cu)
}

/// A fingerprint of a `$*CU` tree, to tell whether the unit's check closures
/// changed it: its node classes, field names and leaf literals. A node's
/// metadata (its origin and resolution) takes no part, so a resolution whose
/// `compile-time-value` holds a closure is never rendered (#17501: rendering
/// one with `Debug` walked the closure's whole environment).
// Cost: O(n), n = nodes and leaves of the tree.
pub(crate) fn tree_fingerprint(cu: &Value) -> u64 {
    use std::hash::Hasher;
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    fingerprint_value(cu, &mut hasher);
    hasher.finish()
}

// Cost: O(n), n = nodes and leaves under `value`.
fn fingerprint_value(value: &Value, hasher: &mut impl std::hash::Hasher) {
    use crate::rakuast::RakuAstFieldValue;
    use std::hash::Hash;
    match value.view() {
        ValueView::RakuAst(node) => {
            std::mem::discriminant(&node.class).hash(hasher);
            node.fields.len().hash(hasher);
            for field in &node.fields {
                field.name.hash(hasher);
                match &field.value {
                    RakuAstFieldValue::Node(child) => fingerprint_value(child, hasher),
                    RakuAstFieldValue::List(items) => {
                        items.len().hash(hasher);
                        for item in items {
                            fingerprint_value(item, hasher);
                        }
                    }
                    RakuAstFieldValue::Adverb(name) => name.hash(hasher),
                }
            }
        }
        ValueView::Mixin(inner, _) => fingerprint_value(inner, hasher),
        ValueView::Int(i) => i.hash(hasher),
        ValueView::Num(n) => n.to_bits().hash(hasher),
        ValueView::Bool(b) => b.hash(hasher),
        ValueView::Str(s) => s.as_str().hash(hasher),
        // Any other leaf (a compile-time value a rewrite embedded) counts by
        // its kind: the nodes around it say where it changed.
        other => std::mem::discriminant(&other).hash(hasher),
    }
}
