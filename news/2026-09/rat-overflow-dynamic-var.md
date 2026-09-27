# `$*RAT-OVERFLOW` now exists and can upgrade a Rat to FatRat

`say $*RAT-OVERFLOW` printed `Nil` instead of `(Num)`, and `$*RAT-OVERFLOW = FatRat`
threw `X::Dynamic::NotFound` even though the variable is meant to be always
available, no `my` declaration required.

The read-side bug was a missing case in `lazy_magic_dynamic_var`
(`src/runtime/io_env.rs`): unlike `$*TOLERANCE`/`$*COLLATION`/etc., it had no
default value at all, so a read fell through to the generic "never assigned"
Nil. The assignment bug was more general: `Interpreter::is_var_dynamic` never
recognized any lazily-materialized builtin dynamic as dynamic, so a bare
`$*x = ...` tripped `CheckDynamicVarDeclared`'s "not declared" guard even
though the read side, and the compiler's own `X::Dynamic::Postdeclaration`
check, already treat the same whitelist (`is_builtin_dynamic_var`) as
always-declared. That also silently affected `$*TOLERANCE`, `$*COLLATION` and
`$*SPEC`; `$*TOLERANCE = 0` now works the same way.

The overflow behavior itself is implemented too: when a `Rat` arithmetic
result's reduced denominator exceeds uint64 range, mutsu used to always
degrade it to a lossy `Num`. It now consults `$*RAT-OVERFLOW` at each of the
five binary arithmetic opcodes (`+`, `-`, `*`, `/`, `**`) and upgrades to
`FatRat` instead when it resolves to the `FatRat` type object, honoring both
top-level (`$*RAT-OVERFLOW = FatRat`) and lexically-scoped
(`my $*RAT-OVERFLOW = FatRat`) assignment. The decision itself still lives in
`make_big_rat_arith`, the single primitive shared by every arithmetic call
site (ADR-0117/ADR-0118); since that function has no `Interpreter` access, the
resolved value is relayed into it via a thread-local RAII guard
(`RatOverflowScope`) set for exactly the duration of the guarded opcode call,
and read only when an operand is already Rat-family, so ordinary Int/Num
arithmetic pays nothing extra. Full custom `UPGRADE-RAT` class support (the
general case in `Language/variables.rakudoc`) is not implemented; only the
documented `Num`/`FatRat` defaults are. (#9778)
