# The nominal conformance check compares interned symbols

`nominal_type_conforms_general` (the fallback of the multi-candidate narrowness relation, ~25k calls
per Red `^create`) tested `Mu` / `Any` and the numeric-role ancestor table through
`Symbol::with_str` string reads, five or more per call. It now compares the interned symbols
(`wk::mu()`, `wk::any()`, and one equality test per numeric role before touching the table), with the
same answers.

Real Red 0.2.7 `Bla.^create`, profiling build, callgrind slope N=5 -> 15, both sides built from
`origin/main` c25b0629: 679.7M -> 675.0M Ir per create (-0.7%). Small: the function is still
~25M Ir per create in `nominal_type_conforms` itself (the memoised fast path declines outside a
ranking scope), so the lever is calling it less, not making each call cheaper.
