//! Core rows 40..58 of the static class catalogue (ADR-19206 slice 2):
//! their `ClassDef` literals, moved verbatim out of `build_builtin_registry`.
//! See `builtin_class_catalogue_core`.

use super::*;
use crate::value::ValueMap;

/// The class of core row `index` (one of rows 40..58).
// Cost: O(s), s = the size of the class's literal sets.
pub(super) fn build(index: usize) -> ClassDef {
    match index {
        // X::Parameter
        40 => ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::Parameter", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // X::Parameter::InvalidConcreteness
        41 => ClassDef {
                // Despite the name, this does NOT inherit `X::Parameter` in
                // rakudo (`.^parents(:local)` is `Exception` directly;
                // `X::Parameter` is a bare package, not a class -- ADR-0029).
                parents: vec!["Exception".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::Parameter::InvalidConcreteness", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // X::Supply::Combinator
        42 => ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::Supply::Combinator", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // X::TypeCheck
        43 => ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::TypeCheck", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // X::TypeCheck::Argument
        44 => ClassDef {
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::TypeCheck::Argument", "X::TypeCheck", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // X::TypeCheck::Assignment
        45 => ClassDef {
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::TypeCheck::Assignment", "X::TypeCheck", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // X::TypeCheck::Binding
        46 => ClassDef {
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::TypeCheck::Binding", "X::TypeCheck", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // X::TypeCheck::Binding::Parameter
        47 => ClassDef {
                parents: vec!["X::TypeCheck::Binding".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&[
                    "X::TypeCheck::Binding::Parameter",
                    "X::TypeCheck::Binding",
                    "X::TypeCheck",
                    "Exception",
                ]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // X::TypeCheck::Return
        48 => ClassDef {
                // Real superclass is `X::TypeCheck` (ADR-0029, verified
                // against raku) -- unlike `X::TypeCheck::Argument` /
                // `::Assignment` / `::Binding` above, this one was missing
                // the intermediate ancestor.
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::TypeCheck::Return", "X::TypeCheck", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // __ChannelSupply
        49 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                // The per-tap producer of a `Channel.Supply`, and the closer
                // that detaches the tap -- see `native_methods::channel_supply`.
                native_methods: [
                    "__mutsu_channel_supply_start",
                    "__mutsu_channel_supply_close",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["__ChannelSupply"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // __PromiseSupply
        50 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                // The per-tap producer of a `Promise.Supply`, and its `.then`
                // settler -- see `native_methods::promise_supply`.
                native_methods: [
                    "__mutsu_promise_supply_start",
                    "__mutsu_promise_supply_settle",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["__PromiseSupply"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // __ScheduledTapPump
        51 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                // ADR-0028 Slice 1: the callback-shim target `Supply.
                // schedule-on()` taps invoke instead of the real tap/done/
                // quit callback — see `native_methods::scheduled_tap_pump`.
                native_methods: [
                    "__mutsu_scheduled_emit",
                    "__mutsu_scheduled_done",
                    "__mutsu_scheduled_quit",
                    "__mutsu_scheduled_run_cue",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["__ScheduledTapPump"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // __SupplyCollector
        52 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                // ADR-0031 Decision B (Slice 2): the emit/done/quit shim
                // `Interpreter::supply_collect_values` taps a Supply with,
                // instead of the old synchronous-replay helpers — see
                // `native_methods::supply_collector`.
                native_methods: [
                    "__mutsu_collector_emit",
                    "__mutsu_collector_done",
                    "__mutsu_collector_quit",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["__SupplyCollector"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // __SupplyDerive
        53 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                // The producer and forwarders of a `Supply.grep`/`.map` over an
                // on-demand source — see `native_methods::supply_derive`.
                native_methods: [
                    "__mutsu_derive_start",
                    "__mutsu_derive_emit",
                    "__mutsu_derive_done",
                    "__mutsu_derive_quit",
                    "__mutsu_derive_close",
                    "__mutsu_stage_emit",
                    "__mutsu_stage_flush",
                    "__mutsu_promise_emit",
                    "__mutsu_promise_done",
                    "__mutsu_promise_quit",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["__SupplyDerive"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // __SupplyDeriveMulti
        54 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                // The producer and forwarders of a live `merge` / `zip` /
                // `zip-latest` recipe — see `native_methods::supply_derive_multi`.
                native_methods: [
                    "__mutsu_multi_start",
                    "__mutsu_multi_emit",
                    "__mutsu_multi_done",
                    "__mutsu_multi_quit",
                    "__mutsu_multi_close",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["__SupplyDeriveMulti"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // __SupplyQuitForwarder
        55 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                // The `quit =>` callback a `whenever` on a chained on-demand
                // source hands to the inner tap, so an unhandled source quit
                // terminates the enclosing supply block (ADR-0031 Decision A)
                // — see `native_methods::supply_quit_forwarder`.
                native_methods: ["__mutsu_supply_forward_quit"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["__SupplyQuitForwarder"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // utf16
        56 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["utf16"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        // utf8
        57 => ClassDef {
                parents: Vec::new(),
                attributes: Default::default(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["utf8"]),
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
