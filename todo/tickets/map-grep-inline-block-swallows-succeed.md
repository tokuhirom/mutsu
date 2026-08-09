# `when`/`default` inside a map/grep block: inline fast path does not absorb the succeed signal

## Affected tests

- `t/http-request-serializer.rakutest` (Cro::HTTP dist) — aborts after `ok 15`,
  before test 16 "multipart/form-data with list of pairs" (test line 270).
  stderr: `Type check failed for return value; expected Supply but got Any
  (Cro::HTTP::Body::MultiPartFormData::Part())`. raku runs the file to
  completion (`1..17`, rc=0).

## Repro

One-liner (verified 2026-08-09, release binary):

```
$ mutsu -e 'my @a = (1,2).map({ when Int { "int" } }); say @a.join(",")'
Runtime error:            # empty message; exit 1
$ raku  -e 'my @a = (1,2).map({ when Int { "int" } }); say @a.join(",")'
int,int
```

`grep` is equally affected: `(1,2).grep({ when Int { True } })` → same empty
"Runtime error:". A direct closure call is NOT affected
(`my $b = { when Int { "int" } }; say $b(5)` prints `int` in both).

The exact Cro shape is `tmp/tapdiag-ser7.raku` (class SerB): a method with
`--> Supply` whose body does `my @parts = @body.map: { when Pair {
Part.new(...) } }` fails with the observed "Type check failed for return value;
expected Supply but got Any (Part())" — the escaping succeed signal carries a
`return_value`, and the method-return machinery misreads it as the routine's
return value, which then fails the `--> Supply` return-type check. (SerA in the
same file proves the nested `self.serialize: $m, FormData.new: parts => ...`
colon-call chain itself is fine.)

The real-world source is
`Cro/HTTP/BodySerializers.rakumod:189-204`
(`Cro::HTTP::BodySerializer::MultiPartFormData.serialize(@body)`), which maps
pairs through exactly this `when Pair / when Part / default` block.

## Root cause

A matched `when` raises a succeed control signal carrying the body's value:
`exec_when_op`, src/vm/vm_given_when_ops.rs:401-407 (`RuntimeError::
succeed_signal()` with `return_value = Some(last)`); same for `exec_default_op`
at :440-446. The signal is normally absorbed at the enclosing block boundary:

- closure calls: src/vm/vm_closure_dispatch.rs:793-803 (`Err(e) if
  e.is_succeed()` → treat `e.return_value` as the block's return value);
- statement bodies: `SucceedBarrier` (src/vm/vm_control_ops.rs:333-357).

But `.map`/`.grep` with a block do NOT invoke the block through that machinery.
The fast path `eval_map_over_items` (src/runtime/resolution_map_grep.rs:416-523)
runs the block's compiled code inline via `vm.run_reuse(&code, ...)` (line 459).
Its error arms handle only `is_next` (:485) and `is_last` (:486); the succeed
signal falls into the generic `Err(e)` arm (:487-502) and propagates out of the
map as a RuntimeError whose message is empty — hence the bare "Runtime error:".

gdb-verified chain: `exec_when_op` (vm_given_when_ops.rs:407 raise) ←
`run_reuse` ← `eval_map_over_items::{closure#11}` (resolution_map_grep.rs:459)
← `dispatch_map_method` — no succeed-absorbing frame in between.

## Fix direction

In `eval_map_over_items` (src/runtime/resolution_map_grep.rs:459-503), add an
arm before the generic error arm, mirroring vm_closure_dispatch.rs:793-803:

```rust
Err(e) if e.is_succeed() => {
    let val = e.return_value.unwrap_or(Value::NIL);
    let val = vm.reify_finite_pipe_value(val)?;   // same post-processing as Ok arm
    match val.view() {
        ValueView::Slip(elems) => result.extend(elems.iter().cloned()),
        _ => result.push(val),
    }
}
```

Also reset the when-matched flag the way `exec_succeed_barrier_op` does
(`set_when_matched(saved)`, vm_control_ops.rs:343-352) so an enclosing `given`
does not see a stale match flag; capture `saved_when_matched` before the
per-item `run_reuse` and restore it in this arm.

Audit the sibling inline runners for the same missing arm (all found by
`grep -n "is_next" src/runtime/resolution_map_grep*.rs`):

- src/runtime/resolution_map_grep.rs:721-722 (second inline loop, `'body_redo`)
- src/runtime/resolution_map_grep.rs:822-823 (`first`-style probe: a succeed
  from a matched `when` means "this item produced a value" — decide truthiness
  from `e.return_value`)
- src/runtime/resolution_map_grep_rw.rs:283-286 and :546-547

For grep, the absorbed `e.return_value` is the predicate result (use its
truthiness), matching raku (`(1,2).grep({ when Int { True } })` → `(1 2)`).

Risk: low — the arm only fires where today the whole map dies with an empty
error. Make sure NOT to absorb succeed in the `for`-loop lazy paths that already
break on it deliberately (src/vm/vm_for_loop_lazy.rs:148 — statement context,
value dropped; that is correct for `for`).

## Verification

- The one-liners above print `int,int` / `(1 2)`-equivalent under mutsu.
- `tmp/tapdiag-ser7.raku`: both A and B print `parts: a,b` and `Supply`.
- `t/http-request-serializer.rakutest`: test 16 no longer aborts the file.
  **Behind this abort sits one more blocker** (measured 2026-08-09 by shadowing
  the `when`-map out of BodySerializers.rakumod): tests 16/17's `like ...,
  /<$expected-output>/` then dies "Prohibited regex interpolation" — see ticket
  `tap-regex-interp-dangerous-heuristic-overbroad.md`. With BOTH fixed the file
  passes 1..17 (verified with both bypass shims; the regex-content assertion of
  tests 16/17 was stubbed in that probe, so re-run the real file after the regex
  fix).
- Add a `t/` pin, e.g. `t/map-when-succeed.t`, covering map+when, grep+when,
  map+default, and the `--> Supply` method shape (SerB).
