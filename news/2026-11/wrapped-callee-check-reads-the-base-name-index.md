# The wrapped-callee check reads the base-name index instead of scanning every routine

When a program has wrapped any routine (Red wraps some), every call of a lexically imported
routine asked `wrapped_sub_is_the_callee` whether the call reaches the wrapped routine. That check
takes `&self`, so its multi-candidate probe (`has_multi_function_unindexed`) walked the whole
functions map (~1.5k keys) on each call -- about 52 times per Red `Bla.^create`. The probe now
reads the base-name index the `&mut` probes already filled, as its sibling
`has_multi_candidates_unindexed` does, and falls back to the full scan only when the index has no
entry for the name's base.

Red `Bla.^create` (real Red 0.2.7, profiling build, callgrind slope N=5 -> 15): 700.4M -> 678.8M Ir
per create (-3.1%). The rest of Red's cost is still flat (`Symbol::as_str` 8%, `nominal_type_conforms`
4%, `Symbol::intern` 4%, malloc/free ~15%).
