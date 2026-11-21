# Symbol-derived memos are flat, symbol-indexed tables

Seven memos of a pure function of a symbol's text kept their answers in a
thread-local hash map (or a thread-local `RefCell<Vec<u8>>`): a package's
parent (`package_parent`, walked once per enclosing package by every
package-chain lookup), `unqualified_part`, `qualified_var_package`,
`term_key_sym`, `lexical_type_key_sym`, `MetaNs::key` (the `__mutsu_*`
metadata keys) and `qualified::flags::of`. A hit paid a thread-local access, a
hash of the key, a `RefCell` borrow and a probe — ~30-60 instructions — and an
AttrX::Mooish `t/090-concurrent.custom` round makes ~1,800 such probes.

They now use the process-global, lock-free tables indexed by symbol id that
`Symbol`'s own flags already use (`SymbolSymMemo`, `SymbolMemo`; one table per
`MetaNs` namespace). Symbol ids are global and append-only and each function
is pure, so one table serves every thread and a racing writer stores the same
answer; a hit is one atomic load. `SymbolSymMemo` gained `get_with` (a compute
that captures the namespace's prefix) and `get_opt` (an answer that may be "no
symbol", such as a top-level package's parent).

The Mooish probe (callgrind slope R=120→360, profiling build) went from
6,126,191 to 6,026,804 instructions per round (−1.62%).
