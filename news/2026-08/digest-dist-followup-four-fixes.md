# Four more general fixes on the way to the `Digest` distribution

CI on the `Digest` dist PR reported four failing roast files. Each turned out to
be an independent general bug — two of them pre-existing, only surfaced because
the `@$x` lowering fix stopped papering over them.

## `:ov` / `:ex` left `$/` as `Nil` instead of an empty List

`roast/S05-modifier/exhaustive.t` (tests 48, 104).

`:g`, `:ov` and `:ex` all make a match return a **List** of `Match`es, so a
failure leaves `$/` an *empty List* and `+@$/` is 0. Only the `:g` path did
that; `:ov` and `:ex` called `clear_match_state()`, which sets `$/` to `Nil` —
correct for a plain match (which returns a single `Match`), wrong here, because
`@(Nil)` is `(Nil,)`, one element. The three sites now share a new
`clear_multi_match_state()`. Pinned by `t/multi-match-empty-result.t`.

## An `@`-sigil read of a Buf/Blob variable unwrapped the container

`roast/S02-types/is-type.t` (tests 1, 3).

`@$blob` lowers to `$blob.list`, so it never reaches `OpCode::GetArrayVar`. What
does reach it is a genuine `@`-sigil variable whose container *is* a Buf
(`my @a is Buf`) — and there `@a` must stay the Buf itself so `@a ~~ Buf` holds.
The Buf-to-element-list arm added to `GetArrayVar` was therefore both unnecessary
and wrong; removing it leaves `t/array-sigil-scalar-deref.t` passing.

## A placeholder in a pointy block was diagnosed too late

`roast/S04-declarations/implicit-parameter.t` (test 16).

A pointy block always has an explicit signature — even `-> { … }`, which
declares zero parameters — so a placeholder written directly in its body cannot
become its parameter the way it would in a bare `{ … }` block. mutsu knew that,
but emitted the diagnosis as a `Die` at the point where the closure literal is
*evaluated*; buried inside a routine that is never called (`sub () { -> { $^a
}.() }`), it never fired. The check now runs in the parser, in `arrow_lambda`,
reusing the same `placeholder_overrides_signature_error` the `sub` declaration
check uses — a compile-time error, as in Rakudo.

Two supporting fixes made the diagnosis survive the trip out of an expression:

* `merge_expected_messages` now returns a **fatal** error verbatim. Pushing a
  context description in front of it both buried the diagnosis inside an
  "expected A or B or FATAL:…" list and silently demoted the error to a
  recoverable one (`PError::is_fatal` only inspects the first message), so the
  enclosing alternation went on to try other productions.
* `placeholder_overrides_signature_error` now also spells its message in the
  `"X::Type: text"` convention. The `map_err` sites that rebuild a `PError` drop
  its structured `exception`, and the convention is what keeps the class from
  being downgraded to `X::Syntax::Confused` there.

`t/signature-placeholder.t` asserted that `sub g() { -> { $^y } }` lives; rakudo
rejects it, so the local test was corrected (roast is authoritative) and the
pointy-block case added. New pin: `t/pointy-block-placeholder.t`.

## `$c[i]++` incremented a *shadowed* outer lexical

`roast/integration/advent2013-day08.t` (test 10).

The oldest of the four, and the reason the test used to pass by accident: it
iterates `@$vec`, which used to resolve to the file-scope array `@vec` rather
than to the block's `$vec`, so the corrupted `$vec` was never read.

`OpCode::PostIncrementIndex` and its three siblings carried only the base
container's *name*, and the VM located the container with a by-name search over
`code.locals` — which picks the FIRST slot with that name. A bare block shares
its enclosing frame's locals, so an inner `my $c` shadowing an outer one gets a
second slot under the same name, and `$c[0]++` mutated the **outer** container
while `$c[0] = …` (which already baked a `target_slot`) mutated the right one.

All four index inc/dec opcodes now carry the same §1.5 compile-time-resolved
slot, threaded into the container read and the in-place writeback through new
`gate_local_slot_at` / `gate_local_slot_value_at` helpers. Pinned by
`t/index-incdec-shadowed-lexical.t`.
