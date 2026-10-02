# Regex capture types and the temporal core move below the runtime

Third slice of the layer decomposition (#10779). `Value`'s lazy `Match`
named the regex engine's capture types (`RegexCaptures`, `CapNode`,
`PosSlot`, `NamedCaptureMap`, `MatchTarget`, ...) through `crate::runtime`,
and its `Str`/`eqv` of `Date`/`DateTime` reached into
`builtins::methods_0arg::temporal` — two module cycles with the layers above
`Value`.

- The capture half of `runtime/regex_types.rs`, together with
  `runtime/regex_named_caps.rs` and `runtime/match_target.rs`, is now
  `value::regex_caps` (`cap_node`, `captures`, `named_caps`, `match_target`),
  with the `:ignoremark` subject view (`strip_marks_text`) and the lazy-Match
  `MUTSU_VM_STATS` counters beside it. The runtime re-exports the set, so the
  regex engine keeps its paths.
- The `MUTSU_VM_STATS` switch is a leaf module (`stats_gate`) that both
  `vm::vm_stats` and the lower-layer counters read.
- The pure civil-date / ISO 8601 / leap-second helpers are
  `value::temporal_core`, re-exported from the builtins' `temporal` module
  (which shrinks from 947 to 735 lines).

`make check-layer-deps` goes from 165 to 135 upward references; no behavior
changes.
