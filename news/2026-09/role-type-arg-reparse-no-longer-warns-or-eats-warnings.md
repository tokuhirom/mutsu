# A parametric role's type-argument re-parse no longer warns, nor eats the unit's warnings

`todo/tickets/eval-declared-my-role-leaks-and-shadows-a-later-lexical-role.md`
reported that a `my role` declared inside `EVAL` leaked into the process-wide
role registry and shadowed a later, lexically scoped role of the same short
name, so the later role's methods were not found.

That headline symptom is gone — the ticket's repro and its `Test`-based form
both agree with rakudo now, and `t/parametric-role-of-type.t` passes 14/14 under
`MUTSU_REAL_TEST=1`. What was still wrong was the noise the ticket's own repro
showed alongside it:

```
$ mutsu -e 'EVAL q|my role R1[::T] { }; my R1 of Int $x = R1[Int].new;|; say "done"'
Useless use of constant value Int in sink context
    at .../EVAL_0:1
done
```

## Root cause

Instantiating a parametric role re-parses each type argument's *source string*
at run time: `ensure_parametric_role_pun_class` → `resolve_role_candidate` →
`eval_role_arg_values`, which called `parse_dispatch::parse_source("Int")`.

`parse_source` is the nested-*compilation-unit* entry point, so that one-word
fragment was run through the full mainline pipeline, including
`sink_warn::add_sink_warnings`. A lone `Int` statement is a bareword type name,
which `describe_useless` classifies as a useless constant — hence the spurious
warning. It only became visible under `EVAL` because the mainline's warning
buffer had already been drained by the time the role was instantiated, whereas
`EVAL`'s drain happened after.

The same call had a second, quieter effect: `parse_program` clears
`PARSE_WARNINGS` up front, so the fragment re-parse *discarded* warnings the
enclosing unit had genuinely collected. `EVAL q|my role R1[::T] { }; 42; my R1
of Int $x = R1[Int].new; 1|` lost rakudo's real `Useless use of constant integer
42 in sink context` and printed the bogus `Int` one in its place.

## Fix

Added `parse_dispatch::parse_fragment` (backed by `parser::parse_fragment`) for
internal re-parses of an expression fragment, as opposed to a compilation unit.
It arms a one-shot `SUPPRESS_SINK_WARNINGS` flag that `parse_program` consumes
next to the existing `EVAL_VALUE_TAIL` flag — a fragment's statements are not in
mainline sink context, so the analysis is skipped entirely — and it saves and
restores `PARSE_WARNINGS` / `VCS_CONFLICT_MARKERS` around the nested parse so
the enclosing unit keeps its own diagnostics. `eval_role_arg_values`'s two
re-parse sites now use it.

Both `EVAL` cases above now match rakudo exactly, including the warning text.

Pinned by `t/role-type-arg-reparse-no-sink-warning.t` (4 tests: no spurious
warning with and without `EVAL`, a real warning surviving a later fragment
re-parse, and the ticket's original shadowing repro).
