# A diagnosed parse error keeps its exception class

The parser already recognises a good number of constructs precisely enough to
name the Raku exception class that rejects them, using the `"X::Type: text"`
message convention that `news/2026-08/typed-exception-class-from-the-message-convention.md`
made load-bearing:

```rust
return Err(PError::expected_at(
    "X::Syntax::CannotMeta: Cannot do . because it is too fiddly",
    r,
));
```

The class did not survive the trip out. Two layers flattened it:

1. The statement-list loop (`parser/stmt/stmtlist.rs`) stringifies a failed
   statement's error into `"expected statement at line N (after M stmts): {e} —
   near: …"`, so the classified message became a *substring* of a longer one.
2. `parse_program` then wrapped that in `"Confused. parse error at line L,
   column C: …"`.

The convention only fires on a message that *starts* with `X::`, so what user
code saw was `X::Syntax::Confused` with the real diagnosis quoted somewhere in
the middle of an "expected A or B or C or …" list.

Both layers now propagate a classified alternative instead of flattening it.
Every message merged into a `PError` shares the same furthest failure position
(`update_best_error` merges only at an equal score), so a message that names a
class describes *this* failure and is strictly better than the generic wrapper.
An error with no such alternative is unchanged — it still renders "Confused."
and classes as `X::Syntax::Confused`.

This was the largest single cluster in the real-`Test` roast residue: 17 of the
145 files that lost individual assertions did so on `throws-like … , X::Syntax::…`
(`todo/tickets/vendor-real-test-module.md`). It closes the `X::Syntax::CannotMeta`
group — `roast/S03-metaops/not.t`, `roast/S03-metaops/zip.t` and
`roast/S03-operators/is-divisible-by.t` now pass under `MUTSU_REAL_TEST=1`, and
`roast/S03-metaops/cross.t` and `roast/S03-operators/arith.t` get past it (both
are left on an unrelated `Test::Util` `group-of` failure). The `X::Comp::Group`
and `X::Syntax::Missing` groups are untouched: mutsu does not raise those classes
at all yet, which is separate work.

Pin: `t/parse-error-keeps-its-exception-class.t`, which also pins that the
undiagnosed fallback is still `X::Syntax::Confused`.
