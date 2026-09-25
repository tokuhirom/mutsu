# A proto body's return no longer drops rebinds of outer-scope variables

P5each's `t/01-basic.t` timed out after two tests (#9336). Its `each(@array)`
is an exported `proto` whose `multi` mixes a role declared in its own body
into the caller's array (`@array does EachArray`), so that the next call sees
the advanced `$!index`. Under mutsu every call saw a fresh, un-mixed array and
`while each(@a) -> ($k, $v) { ... }` looped on `0 a` forever.

The issue's narrowing ("only in a `while` condition, only when a statement
follows the loop, only for an imported multi") turned out to be incidental.
The real conditions are two:

- the call goes through the interpreter's `call_proto_function` path, which
  runs the proto body and then restores the caller's env, carrying over the
  new values of the names the body rebound (a `multi` whose body declares a
  role is not eligible for the direct multi fast path, so it lands here);
- the caller's variable is visible from the call site only through a *parent*
  env tier, which is the case for any variable of an enclosing scope seen from
  inside a bare block or a loop body (the trailing statement in the issue's
  repro just changed how the loop was lowered into such a scope).

`restore_env_preserving_existing` walked `saved_env.keys()` to decide what to
carry over, and `Env::keys` exposes only an env's own overlay tier. A name the
caller reaches through its parent chain was therefore never considered, and
the mixin written into it by the multi was discarded when the proto returned.
The carry-over now also walks the current env's overlay, which holds every
name the body wrote, and keeps any of them the caller can already see; a name
the proto body declared itself (or a proto parameter) still does not leak into
a same-named caller lexical.

Pinned by `t/routines/dispatch/proto-body-writeback-through-parent-tier.t`,
which includes the two-file P5each shape via `t/lib/ProtoMultiEachMixin.rakumod`.
