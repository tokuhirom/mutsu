# A method call no longer re-interns its locals and the deferral names

`call_compiled_method_in_unit` seeded the frame's locals with `Env::get(&str)` and wrote
them back with `Env::insert(String)` — an interner round trip (plus a `String` clone on the
write side) per local per call — although the chunk already carries `locals_sym`. It now
reads and writes through the pre-interned symbols. `resolve_deferral_expansion` interned the
same method name once per MRO level and pass (`own_overloads_at_level`,
`method_entry_proto`); it now interns it once and probes the registry by `Symbol`
(`user_method_overloads_sym`, `get_method_overloads_with_role_fallback_sym`).

ADR-15737 D2b, measured with `scripts/call-path-ab.sh` (profiling build, A/B against the
same tree's main build, `mooish-lazy`, slope between 10 and 50 iterations): allocations per
iteration 2,393 -> 2,309 (-3.5%, deterministic); instructions per iteration 4.18M -> 4.10M
(-1.9%, but the Mooish loop's own run-to-run spread is about +-2%, so treat the instruction
figure as "not slower"). `plain-call` is unchanged within noise (33,948 -> 33,906 Ir).
This is a 1-2% slice; it is the first of the call-path slices, not progress toward the
#15688 goal on its own.
