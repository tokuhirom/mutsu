# `x`, `~` and interpolation build their result as strands

A `Str` payload used to be one flat buffer (`Arc<String>`), so every string
producer paid for its whole result: `"あ" x $n` wrote `3 * $n` bytes, `$a ~ $b`
copied `$a` whenever anything else still held it, and `"$a-$b"` copied every
part and then ran NFC over the lot. MoarVM builds a *strand* list instead --
a few references to flat strings, each with a repeat count -- and writes the
characters out only when something reads them.

[ADR-0120](../../docs/adr/0120-str-payload-may-be-a-lazily-flattened-strand-list.md)
gives mutsu the same representation. The payload is now `Arc<StrBody>`, which
is either `Flat(String)` or a lazily flattened strand list. `StrBody` derefs
to `String`, so the ~2,800 places that read a `Str` still read a `&str` and
did not change; the first read of a strand list flattens it once into a cache.
Strand bases are always flat (the list is one level deep) and a list holds at
most 64 strands, MoarVM's bound; a join is only stranded where it cannot
compose under NFC, so no result changes.

- `x` / `nqp::x` is one repeat strand over its source: O(n) in the source
  instead of O(n * count) (#9253). It now also enforces Rakudo's deterministic
  4294967295-grapheme cap with Rakudo's two messages, so an oversized request
  is a catchable error at the `x` rather than an allocation failure at first
  read; that retires the old fallible-reservation TODO.
- `~` / `nqp::concat` with a shared (or strand) left operand references both
  operands: O(1) instead of O(n1 + n2). A left operand held by nothing else is
  still grown in place (amortized O(n2)).
- Interpolation references every `Str` part of at least 256 bytes and copies
  only the rest: O(parts) plus the copied bytes, instead of O(total) plus a
  whole-result NFC pass.

Results under 1 KiB stay flat: a copy that small is cheaper than the later
flatten.

Measured on a release build (20 iterations each, second run; baseline is the
same tree before the change):

| case | n = 1M | 2M | 4M |
|---|---:|---:|---:|
| `my $r = "あ" x $n`, before | 0.0080 s | 0.0153 s | 0.0365 s |
| after | see PR | | |

`scripts/str-complexity-check.sh` gained three cases (shared-left `~`,
interpolation with a shared part, `x` with a growing count), and
`t/types/string/str-strands.t` pins that no strand result is observable.

The remaining `~=` shapes of #9209 (a closure-captured local and `$!attr`)
still copy on every append; they need the in-place path extended through a
cell, not strands, and are the next slice, with #9161 (`[~]`) and #9252
(sharing `Arc<StrBody>` as Pair / hash keys).
