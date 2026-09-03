# "mutsu does not have this routine" is now a diagnostic, and the plan for saying it about methods was wrong

ADR-0065 S2 is the capability that makes a language server *for mutsu* worth
building: a construct rakudo accepts and mutsu does not is a true positive here,
and an agent writing Raku for mutsu otherwise has exactly one way to find out —
run it and see what breaks.

The ADR said the work was to make mutsu's built-in names enumerable: "they are
currently string literals in `match method { ... }` arms ... the fix is to derive
the dispatch arms and a name table from one source." Both halves of that turned
out to be wrong, in opposite directions.

## The table already existed, and is the wrong shape for a diagnostic

`src/builtins/native_method_row.rs` — ADR-0019 Phase E, already read in
production by `.^methods` and `.^can` — is an `(owner, name)` catalog with
per-arity recognition flags. No enumeration work was needed.

It cannot back a diagnostic, though, because it is **deliberately conservative in
the direction that produces false positives**. A pair with no row reports "not
servable", and whole owners are uncovered by construction: `Sub`, `Signature`,
`IO::Path`, `IO::Handle`, `Cool`, and most of `Any`/`Mu`. Absence means "the
2026-08-10 probe did not classify this", not "mutsu does not have it". Reporting
absence as a defect would tell an agent that a method mutsu implements does not
exist — the one failure D5 says is unrecoverable, because the agent believes it.

## And the real blocker is the receiver type, which the ADR did not account for

`$x.foo` cannot be judged without knowing what `$x` is, and mutsu's AST carries
no type information for the same reason it carries no positions. The honest scope
for method diagnostics is the subset where the receiver is statically known — a
literal, or a bareword type object — plus a table that distinguishes "known
absent" from "unclassified". That is a separate slice with a real design question
in it, and it is not what shipped.

## The routine half has no receiver, and mutsu already had it

A call with no receiver has no ambiguity, so the signal is available immediately
there. And `src/runtime/undeclared_routines.rs` already implements rakudo's
CHECK-time `X::Undeclared::Symbols` scan, with exactly the contract a diagnostic
needs written in its own module docs: declarations are collected scope-blind
across the unit, the check abandons a unit that imports names it cannot see
through, and *"a missed construct can only produce a false negative, never a false
positive"*.

So S2 wires that existing analysis into `analysis::check`. `nosuchsub()` now
comes back as `code: "UndeclaredRoutine"` on the right line, with mutsu's own
"Did you mean" attached.

## Building it exposed two things worth having found

**Constructing an `Interpreter` costs 9.2 ms and retains 7.2 KiB.** The obvious
implementation was to build one and call the runtime's own
`check_undeclared_routines_mainline`. Measured over 4000 construct-and-drop
cycles on a debug build, that is twice the cost of parsing the whole document and
fifteen times its memory — linear, and unaffected by `MUTSU_GC=on`, so not a GC
cycle waiting for a collector. Paid on every keystroke, it would have made the
server's memory profile sixteen times worse than a plain parse.

Every lookup the runtime path adds is per-interpreter registry state that a
*fresh* interpreter has none of, so the verdict is identical without one. The
static predicates moved into a single shared function — one list, so the two
paths cannot drift — and the frontend calls
`check_undeclared_routines_without_interpreter`. Analysis is now 5.0 ms per
document retaining 0.52 KiB: the same memory profile as a plain parse, and
cheaper than `dump_ast`, which additionally formats the AST into a string. The
interpreter-construction cost is recorded on its own
(`todo/perf/interpreter-new-is-expensive-and-retains-memory.md`); it is not an
LSP problem.

**mutsu did not suggest the unit's own routines.** D4 asks for the replacement to
travel with the diagnostic, and mutsu already computed one — for core routines
only. `sub greeting() { }; greetng()` reported the typo with no way to see what
was meant, where rakudo answers "Did you mean 'greeting'?". The candidates came
from the interpreter's registry, which does not hold the unit's declarations at
the point the check runs, while the walker had already collected them.

The walker now tracks routine declarations separately from the names it collects
in order to *suppress* calls. That distinction is the whole subtlety: the
suppressing set deliberately absorbs variables and types, because suppressing on
any of them is the safe direction — but drawing suggestions from it would offer a
`my $greeting` as the routine you meant, which rakudo never does.
`t/undeclared-routine-suggests-unit-own-subs.t` pins both halves and passes
unmodified under real raku.

That is the D7 property working as intended: the language server's requirements
improving mutsu's own diagnostics rather than taxing them.
