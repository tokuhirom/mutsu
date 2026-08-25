# `Backtrace` gains `next-interesting-index`, `outer-caller-idx` and `nice`

All three documented `Backtrace` introspection methods raised
`No such method '<name>' for invocant of type 'Backtrace'`. The finding came
from the doc-diff harness (`Type/Backtrace.rakudoc:72,84,96`):

```raku
sub zipi { { { die "Something bad happened" }() }() };
try zipi;
say $!.backtrace.next-interesting-index;            # raku: 2
say $!.backtrace.next-interesting-index( :named );  # raku: 4
say $!.backtrace.outer-caller-idx( 4 );             # raku: [6]
say $!.backtrace.nice( :oneline );                  # raku: "  in sub zipi at ... line 1"
```

## Semantics established against `raku` first

The docs describe these three in one sentence each, and two of the four
behaviours turn out to be genuinely surprising, so the real contract was derived
by probing reference `raku` across five different backtrace shapes before any
code was written.

- **`next-interesting-index(Int $idx = 0, :$named, :$noproto, :$setting)`**
  increments *before* it looks, so `$idx` is a strictly-exclusive starting
  point and the answer is always `> $idx`. It returns `Nil`, not an index, once
  the list is exhausted. `:$named` keeps only frames whose `.code.name` is
  non-empty — and `<unit>` counts as named, since that is literally its name.
  `:$setting` reads backwards from its name: it *includes* setting frames
  rather than hiding them (`next-interesting-index(:setting)` on the example
  above answers `1`, the setting `die` frame, where the unflagged call answers
  `2`).
- **`outer-caller-idx($startidx)`** is **lexical**, not dynamic. For
  `sub a { b() }; sub b { { die }() }`, the frame for `b` reports the `<unit>`
  frame as its outer caller, *not* `a`'s frame — it walks the `.code.outer`
  chain and matches it against the frames below `$startidx`, stopping once it
  has included one routine frame. An out-of-range index answers `($startidx,)`
  and a negative one `()`.
- **`nice(:$oneline)`** returns a `Str`, newline-terminated, and merges an
  anonymous frame with its outer caller: the anonymous frame supplies the
  file/line while the enclosing scope supplies the name, which is where
  `  in block <unit> at ... line N` comes from. `:oneline` is **not** the first
  line of `nice`: it renders exactly one entry starting at the *second*
  interesting frame, i.e. it names where the innermost frame was **called
  from**. That is why the documented answer for the `zipi` example is
  `in sub zipi` rather than the anonymous block that actually died, and why
  `sub inner { die }; sub outer1 { inner() }` answers `in sub outer1`, not
  `in sub inner`. All five probed shapes agree on this rule.

## The implementation, and how it relates to mutsu's frame model

The three methods live in a new `src/builtins/backtrace_methods.rs` and are
reached from the native fast path at every arity they can be called with — the
0-arg cascade (`nice`, `next-interesting-index`), the 1-arg one (a start index,
or a single named flag, which arrives as a `Pair` in the argument list), and the
2-arg one (`next-interesting-index(2, :named)`). No `runtime/methods.rs`
slow-path handler was added.

Everything is computed from the `frames` attribute the `Backtrace` instance
already carries, following the precedent set when positional indexing was fixed
by delegating the subscript to that same list: nothing re-derives a frame list
from interpreter state, so `nice`, the two index methods, `$bt[N]`, `.list` and
`.elems` agree by construction.

Two places needed a decision, because mutsu's frame model differs from Rakudo's
(mutsu has no Raku-written CORE setting, so its `die`/`throw` are Rust functions
with no callframe, and frame 0 is already the innermost *user* frame — see
`todo/tickets/backtrace-frame-indexing-returns-nil.md` for why matching Rakudo's
absolute frame count is deliberately deferred):

- `nice` starts at index **0 inclusive**, where Rakudo starts at 1 — Rakudo's
  index 0 is always a setting frame it means to skip, and mutsu has none. The
  filters themselves are still written in terms of `.is-hidden`/`.is-setting`,
  so they keep working unchanged if mutsu ever grows hidden frames.
- `outer-caller-idx` has no lexical `.outer` link to walk, so the chain is
  reconstructed from the dynamic stack under the two rules that make it
  coincide with Rakudo's answer: an anonymous block's callers are the following
  frames up to and including the first routine (the routine containing it),
  while a declared routine's enclosing scope is the compilation unit, i.e. the
  outermost `<unit>` frame. Checked against `raku`'s actual output for every
  non-setting frame of both probe shapes, this reproduces Rakudo exactly.

`:noproto` is accepted and hides nothing: mutsu's frames record no
`proto`-dispatcher bit.

The pay-off of getting the semantics right rather than merely making the four
repro lines print something is that `nice` and `nice(:oneline)` now come out
**byte-identical** to `raku`'s on the shapes where the two frame lists line up —
e.g. for `sub inner { die }; sub outer1 { inner() }; sub outer2 { outer1() }`,
both produce the same four-line `nice` and the same one-line
`  in sub outer1 at FILE line 2`.

## Coverage

`t/backtrace-introspection.t` (34 assertions) pins all three methods plus the
`.full` line shape, and passes verbatim under both `raku` and `mutsu`. Every
assertion is *relative* — the returned index is a valid, non-hidden index into
`.frames`; `:named` lands on a named frame and never before the unflagged
answer; `outer-caller-idx` returns a `Positional` of valid indices strictly
below the given one, and for a top-level sub is exactly the outermost `<unit>`
frame; `.nice(:oneline)` is a single newline-terminated line naming the calling
routine — rather than hardcoding Rakudo's absolute `2` / `4` / `[6]`, which
mutsu's shorter frame list cannot and should not reproduce.

The three methods (and `full`, which had none) gained rows in the native method
catalog, so `Backtrace.^can('nice')` answers `1` the way it does in `raku`.
