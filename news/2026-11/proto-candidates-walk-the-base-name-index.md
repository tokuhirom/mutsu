# A proto's `{*}` gathers its candidates from the base-name index

`resolve_remaining_proto_candidates`, which every `proto` dispatch with a
`{*}` body runs to find the candidates after the current one, used to walk the
whole function registry twice per call: once for the typed keys
(`Pkg::name/<arity>:<types>`) and once per package for the generic and `__m`
multi keys, calling `Symbol::resolve` on every key on the way. In a Red
workload that was ~956 registry keys per dispatch and 6.5M filter calls for one
`ResultSeq.grep` over `sqlite_master`.

Every key either pattern can match shares the routine's base name, so the
gather now iterates the base-name key index the main resolver already keeps
(`fn_keys_for_base`) — O(candidates of the name) per dispatch instead of
O(registry). The Red migration verifier's `tables-names.grep(...)` went from
~9.2 s to ~7.7 s per evaluation on a release build (#17553).
