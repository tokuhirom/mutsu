//! Core rows 20..40 of the static class catalogue (ADR-19206 slice 2):
//! their `ClassDef` literals, moved verbatim out of `build_builtin_registry`.
//! See `builtin_class_catalogue_core`.

use super::*;
use crate::value::ValueMap;

/// The class of core row `index` (one of rows 20..40).
// Cost: O(s), s = the size of the class's literal sets.
pub(super) fn build(index: usize) -> ClassDef {
    match index {
        // IterationBuffer
        20 => ClassDef {
                parents: vec!["Any".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["IterationBuffer", "Any", "Mu"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Lock
        21 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                // `new` is Lock's own constructor in rakudo (#9340).
                native_methods: std::iter::once("new")
                    .chain(crate::builtins::method_table::handle_names("Lock"))
                    .map(str::to_string)
                    .collect(),
                mro: sym_mro(&["Lock"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Lock::Async
        22 => ClassDef {
                parents: vec!["Lock".to_string()],
                attributes: Default::default(),
                native_methods: crate::builtins::method_table::handle_names("Lock::Async")
                    .map(str::to_string)
                    .collect(),
                mro: sym_mro(&["Lock::Async", "Lock"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Lock::ConditionVariable
        23 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: crate::builtins::method_table::handle_names("Lock::ConditionVariable")
                    .map(str::to_string)
                    .collect(),
                mro: sym_mro(&["Lock::ConditionVariable"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Lock::Soft
        24 => ClassDef {
                parents: vec!["Lock".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Lock::Soft", "Lock"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Mu
        25 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Mu"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Proc::Async
        26 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                // `new` is Proc::Async's own constructor (`new(*@args)`), so it
                // is a local method in rakudo's `.^method_table` -- Test::Mock
                // walks that table to decide what it may override (#9340).
                native_methods: [
                    "new",
                    "start",
                    "command",
                    "started",
                    "w",
                    "pid",
                    "stdout",
                    "stderr",
                    "Supply",
                    "bind-stdin",
                    "bind-stdout",
                    "bind-stderr",
                    "kill",
                    "write",
                    "close-stdin",
                    "ready",
                    "print",
                    "put",
                    "say",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["Proc::Async"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Promise
        27 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: ["keep", "result", "status", "then"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["Promise"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Promise::Vow
        28 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: ["keep", "break"].iter().map(|s| s.to_string()).collect(),
                mro: sym_mro(&["Promise::Vow"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Scheduler
        29 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                // `Scheduler` is a composable role (`class Test::Scheduler does
                // Scheduler {...}`) with NO native implementation of its own --
                // only the concrete schedulers below have one, and each lists
                // `cue` itself. Claiming a native `cue` here made every user
                // class that composes the role look native-backed, so its own
                // `method cue` was bypassed and dispatch died with
                // "No native method 'cue' on 'MyScheduler'".
                native_methods: HashSet::new(),
                mro: sym_mro(&["Scheduler"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Semaphore
        30 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: ["acquire", "try_acquire", "release"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["Semaphore"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Supplier
        31 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: [
                    "emit",
                    "done",
                    "quit",
                    "Supply",
                    "__mutsu_register_close_phaser",
                    "__mutsu_interval_tick",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["Supplier"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Supplier::Preserving
        32 => ClassDef {
                parents: vec!["Supplier".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Supplier::Preserving", "Supplier"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Supply
        33 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: [
                    "emit",
                    "share",
                    "tap",
                    "act",
                    "encode",
                    "decode",
                    "repeated",
                    "do",
                    "reverse",
                    "split",
                    "interval",
                    "tail",
                    "delayed",
                    "min",
                    "minmax",
                    "collate",
                    "lines",
                    "merge",
                    "unique",
                    "on-close",
                    "classify",
                    "categorize",
                    "Channel",
                    "Supply",
                    "Promise",
                    "schedule-on",
                    "native-descriptor",
                    "migrate",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["Supply"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Tap
        34 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: crate::builtins::method_table::handle_names("Tap")
                    .map(str::to_string)
                    .collect(),
                mro: sym_mro(&["Tap"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Thread
        35 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: ["finish", "id"].iter().map(|s| s.to_string()).collect(),
                mro: sym_mro(&["Thread"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // ThreadPoolScheduler
        36 => ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Default::default(),
                native_methods: ["cue", "uncaught_handler", "loads", "max_threads", "usage"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["ThreadPoolScheduler"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // X::AdHoc
        37 => ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::AdHoc", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // X::Coerce::Impossible
        38 => ClassDef {
                // Real superclass is `X::Coerce` (ADR-0029, verified against
                // raku; `X::Coerce` itself is a row of the static class
                // catalogue, ADR-19206).
                parents: vec!["X::Coerce".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::Coerce::Impossible", "X::Coerce", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // X::Numeric::Real
        39 => ClassDef {
                // Real superclass is `X::Numeric::CannotConvert`, not
                // `Exception` directly (ADR-0029, verified against raku).
                parents: vec!["X::Numeric::CannotConvert".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::Numeric::Real", "X::Numeric::CannotConvert", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        _ => ClassDef::default(),
    }
}
