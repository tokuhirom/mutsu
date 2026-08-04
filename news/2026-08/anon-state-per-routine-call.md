# An anonymous state variable resets when its enclosing routine is re-entered

A bare `$` (`$++` / `++$` / `$ ~= …`) is an anonymous state variable belonging
to its enclosing block's **clone**. A named routine's own body is cloned once,
at registration, so a `$` written directly in it keeps counting across calls;
the blocks *inside* that routine are re-cloned on every call, so a `$` in one
of them restarts. mutsu keyed the counter by its compile-time name alone, so it
kept counting everywhere:

    sub f() { (map { ++$ }, 1, 2, 3).join(',') }
    say f();   # raku: 1,2,3   mutsu: 1,2,3
    say f();   # raku: 1,2,3   mutsu: 4,5,6

This was the anonymous-state half of `Digest::RIPEMD`'s wrong second digest
(`todo/tickets/digest-dist-blockers.md` blocker 2): its output stage rotates
the five hash words with `map { $_[[^5].rotate(++$)] }`, so the second and
later `rmd160(...)` calls in one process rotated by the wrong amount.

## The rule

> the counter resets iff the `$` is **lexically inside a nested block** that is
> **lexically inside a routine**, and then it resets once per *call of that
> routine* — not per block iteration.

## Third attempt lucky: classify at the parser's mint site

Two earlier prototypes were shelved (see the history preserved below): a
process-global registry of per-call ids was poisoned by analysis passes that
re-compile the same source without its true lexical context, and a per-chunk
set (PR #5885) split one `$` occurrence across two chunks that classified it
differently — the write landed on one key while the read used another
(`roast/S32-list/classify.t` test 39 caught it).

The fix that works decides the classification exactly ONCE, in the parser, at
the moment the occurrence's unique name is minted, and **bakes it into the
name**: a per-call `$` is spelled `__ANON_STATE_PC_<id>__`. Every later
compilation of that AST — the routine-hoist pass, capture analysis, and the
runtime re-compiles in `eval_map_over_items`/`call_sub_value`/gather — sees the
same name and therefore agrees by construction. No compile-time cursor, no
per-site restatement, no registry.

The parser knows the scope rule structurally: `simple::SCOPES` already pushes a
frame per `{ }`, so routine-declaration parsers (`sub`/`method` bodies, the
anonymous `sub (...) {...}` / `method {...}` forms — which are routine frames
at run time) mark their body scope via `mark_current_scope_routine_body`, and
`anon_state_is_per_call` walks the stack: per-call iff at least one unmarked
block frame sits between the mint site and the nearest routine-body frame.
This is *stronger* than the shelved prototypes, which could not classify
`if C { $++ }` (their AST carries no statement-modifier flag on `Stmt::If`):
at parse time `$++ if C` (no braces — persists) and `if C { $++ }` (a block —
resets per call) are trivially distinct, and both now match rakudo.

**Run time.** `RoutineFrame` gained a monotonic `invocation_id`, and
`anon_state_key` folds the id of the innermost enclosing **non-block** frame
into the state-store key for a `_PC_` name. At the mainline there is no such
frame, so the id is a constant and a top-level `$` keeps counting —
`[ $++ xx 3 ] xx 3` is still `0..8`. A `_PC_` name is also read from the state
store *outright* (`per_call_anon_state_read`, chained in front of the five
read chains): its `env` entry is written by `SetGlobal` and outlives the block
clone, so on the first use in a new call the store legitimately misses while
env still holds the previous call's value — answering the site default on a
miss is part of the contract.

## A second mint site: assignment targets collapsed onto ONE name

Testing `~($ ~= $_)` (the classify-callback shape) exposed that the
assignment-statement parsers (`assign_stmt`, `try_parse_assign_expr`) spelled
EVERY bare-`$` assignment target `__ANON_STATE__` — one shared cell for the
whole process, so `$ ~= "z"` in one sub and `$ += 5` in another corrupted each
other (the `+=` died with "Cannot convert string to number ... 'zzzz'").
Both sites now mint the same unique per-occurrence name the expression parser
gives `$++` (`mint_anon_state_name`), which also carries the per-call
classification. `var_name` itself keeps the collapsed spelling — it also
parses declarations and anonymous signature parameters (`sub f($)`), which
must not change.

## Known residue

An anonymous routine nested inside another routine still persists where rakudo
resets (`sub outer() { my $s = sub { ++$ }; … }` — rakudo re-clones the inner
sub per `outer()` call). The runtime key picks the innermost non-block frame,
which is the inner sub's own invocation; classifying it per-call would reset
per *inner* call, which is worse. Unchanged from before; no roast or `t/` test
exercises it.

Pinned by `t/anon-state-per-routine-call.t` (19 tests, all also passing under
`raku`), covering both directions of the rule row by row, including the
if/while-block rows the prototypes could not classify and the
assignment-target forms.

<details><summary>the shelved-prototype history (from the original ticket)</summary>

- Routing `anon_state_key` through the named-`state` clone-id
  (`scoped_state_key`) broke the named-sub persistence rows: `state_scope_id`
  alternates between two values when read mid-body.
- Route B v1 classified at compile time into a process-global registry keyed by
  occurrence id. The same source is compiled more than once (routine-hoist,
  `record_type_body_captures`, the real body) and those passes do not all
  reproduce the true lexical context; one wrong pass poisoned the sticky
  registry (`roast/S32-list/rotor.t` hung at test 17).
- Route B v2 (PR #5885) moved the set onto `CompiledCode`. Runtime paths that
  re-compile a block body from its AST had to restate the cursor, and then one
  `$` occurrence could be executed by two chunks that classified it
  differently — write on `…#<bucket>`, read on the plain key
  (`roast/S32-list/classify.t` test 39).

</details>
