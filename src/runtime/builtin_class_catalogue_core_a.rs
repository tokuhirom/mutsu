//! Core rows 0..20 of the static class catalogue (ADR-19206 slice 2):
//! their `ClassDef` literals, moved verbatim out of `build_builtin_registry`.
//! See `builtin_class_catalogue_core`.

use super::*;
use crate::value::ValueMap;

/// The class of core row `index` (one of rows 0..20).
// Cost: O(s), s = the size of the class's literal sets.
pub(super) fn build(index: usize) -> ClassDef {
    match index {
        // Any
        0 => ClassDef {
                parents: vec!["Mu".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Any", "Mu"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Backtrace
        1 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Backtrace"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Blob
        2 => ClassDef {
                parents: vec!["Any".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Blob", "Any", "Mu"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Buf
        3 => ClassDef {
                parents: vec!["Blob".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Buf", "Blob", "Any", "Mu"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Cancellation
        4 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: crate::builtins::method_table::handle_names("Cancellation")
                    .map(str::to_string)
                    .collect(),
                mro: sym_mro(&["Cancellation"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Channel
        5 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: ["send", "receive", "close", "closed"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["Channel"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Collation
        6 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Collation"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // CompUnit::Repository::FileSystem
        7 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["CompUnit::Repository::FileSystem"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // CurrentThreadScheduler
        8 => ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Default::default(),
                native_methods: ["cue", "uncaught_handler", "loads"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["CurrentThreadScheduler"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Encoding::Decoder::Builtin
        9 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: crate::builtins::method_table::encoding_names(
                    "Encoding::Decoder::Builtin",
                )
                .map(str::to_string)
                .collect(),
                mro: sym_mro(&["Encoding::Decoder::Builtin"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Encoding::Encoder::Builtin
        10 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: crate::builtins::method_table::encoding_names("Encoding::Encoder::Builtin")
                    .map(str::to_string)
                    .collect(),
                mro: sym_mro(&["Encoding::Encoder::Builtin"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Encoding::Registry
        11 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: ["find", "register"].iter().map(|s| s.to_string()).collect(),
                mro: sym_mro(&["Encoding::Registry"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // Exception
        12 => ClassDef {
                parents: vec![],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // FakeScheduler
        13 => ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Default::default(),
                native_methods: ["cue", "progress-by", "time"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["FakeScheduler", "Scheduler"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // IO::Path
        14 => ClassDef {
                // ADR-0051 P1: `IO::Path` genuinely IS `Cool` in raku
                // (`IO::Path.^mro` is `IO::Path, Cool, Any, Mu`) -- the previous
                // `parents: vec![]` / `mro: sym_mro(&["IO::Path"])` made `.^mro`
                // dead-end at itself (worse, `class_mro_readonly` returns the
                // cached `mro` field directly for a registered class, so leaving
                // it non-empty here permanently short-circuited recomputation).
                // Declaring the real parent and leaving `mro` empty lets
                // `Registry::compute_class_mro`'s ordinary linearization compute
                // and cache the correct `[IO::Path, Cool, Any, Mu]` chain the
                // same way every other class gets its MRO.
                parents: vec!["Cool".to_string()],
                attributes: Default::default(),
                // `new` is IO::Path's own constructor in rakudo (#9340).
                native_methods: [
                    "new",
                    "Str",
                    "gist",
                    "raku",
                    "perl",
                    "IO",
                    "Numeric",
                    "Real",
                    "Int",
                    "Rat",
                    "Num",
                    "FatRat",
                    "basename",
                    "dirname",
                    "cleanup",
                    "parts",
                    "parent",
                    "sibling",
                    "child",
                    "add",
                    "extension",
                    "absolute",
                    "relative",
                    "resolve",
                    "resolve-inside",
                    "volume",
                    "is-absolute",
                    "is-relative",
                    "e",
                    "f",
                    "d",
                    "l",
                    "r",
                    "w",
                    "x",
                    "rw",
                    "rwx",
                    "mode",
                    "s",
                    "z",
                    "created",
                    "modified",
                    "accessed",
                    "changed",
                    "lines",
                    "words",
                    "slurp",
                    "open",
                    "copy",
                    "rename",
                    "move",
                    "chmod",
                    "mkdir",
                    "rmdir",
                    "dir",
                    "spurt",
                    "unlink",
                    "symlink",
                    "watch",
                    "succ",
                    "pred",
                    "CWD",
                    "SPEC",
                    "link",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                // Left empty deliberately (ADR-0051 P1) -- see the `parents`
                // comment above. `Registry::compute_class_mro` computes and
                // caches `[IO::Path, Cool, Any, Mu]` here on first use.
                mro: [].into(),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // IO::Pipe
        15 => ClassDef {
                // `IO::Pipe is IO::Handle`: a pipe satisfies `IO::Handle` constraints and
                // `slurp($proc.err)` reads it instead of opening a path.
                parents: vec!["IO::Handle".to_string()],
                attributes: Default::default(),
                native_methods: ["slurp", "slurp-rest", "Str", "gist", "print", "close"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["IO::Pipe", "IO::Handle"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // IO::Socket::Async
        16 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: crate::builtins::method_table::handle_names("IO::Socket::Async")
                    .map(str::to_string)
                    .collect(),
                mro: sym_mro(&["IO::Socket::Async"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // IO::Socket::Async::ListenSocket
        // The `Tap` a listening socket's supply answers (ADR-11276 §9.157).
        17 => ClassDef {
                parents: vec!["Tap".to_string()],
                attributes: Default::default(),
                native_methods: crate::builtins::method_table::handle_names("IO::Socket::Async::ListenSocket")
                    .map(str::to_string)
                    .collect(),
                mro: sym_mro(&["IO::Socket::Async::ListenSocket", "Tap"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // IO::Socket::Async::Listener
        18 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: ["tap", "act"].iter().map(|s| s.to_string()).collect(),
                // Real raku's `IO::Socket::Async.listen(...)` literally IS a
                // `Supply` (built from a `supply { ... }` block in CORE.setting) --
                // there is no separate "Listener" type. mutsu's listener is a
                // bespoke native object with its own `tap`/`act` handler
                // (`native_socket_async_listener`, dispatched by exact class name
                // in `native_methods/mod.rs`'s IMMUTABLE table, which already
                // special-cases this class name so it is unaffected by the MRO).
                // Adding `Supply` here makes `$listener ~~ Supply` true, matching
                // raku -- needed by consumers that type-check the return value of
                // `.listen()` before tapping it (e.g. IO::Socket::Async::SSL's
                // `!server-setup`, which fell through to treating the listener
                // itself as a single accepted connection when the smartmatch
                // failed). See `dispatch_native_instance_method_mut`'s hardcoded
                // class list in `native_methods/mod.rs`, which must also list this
                // class name explicitly -- its MRO-walk fallback would otherwise
                // now match `Supply` and route `tap`/`act` to the wrong (generic,
                // non-functional) mutable-Supply handler instead of falling
                // through to this class's real immutable one.
                mro: sym_mro(&["IO::Socket::Async::Listener", "Supply"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // IO::Socket::INET
        19 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: crate::builtins::method_table::handle_names("IO::Socket::INET")
                    .map(str::to_string)
                    .collect(),
                mro: sym_mro(&["IO::Socket::INET"]),
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
