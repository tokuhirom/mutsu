# ADR-0119: A Seq whose source can be pulled a prefix at a time is not reified for `.head(n)`

- **Status**: Accepted (2026-09-24, implemented for `Str.comb` / `.lines` / `.words` and
  `IO::Handle.lines` / `.words`; #9251).
- **Deciders**: tokuhirom, Claude
- **Context**: [ADR-0034](0034-seq-reification-is-in-place-and-distinct-from-consumption.md) (a
  `Seq` body is reified in place, and a consuming method steals it once),
  [ADR-0058](0058-map-grep-produce-a-deferred-seq.md) §6 (a pull-granular deferred Seq is
  named there as the natural follow-up).

## 1. Context

`$s.comb.head(3)` took 6.7 s for 20 calls on a 1M-char string and twice that at 2M; rakudo takes
3 ms at either size. `$fh.lines.head(3)` on a 4 MB file took 1.5 s per call and left the handle
at EOF. Both are O(n) where rakudo is O(prefix).

Rakudo's setting shows why (`moar --dump CORE.c.setting.moarvm`, Str.rakumod:937-970):
`Str.comb` is `Seq.new(<iterator>.new(self))`, and that iterator keeps `$!str` and a `$!pos`
cursor. `pull-one` is `++$!pos < chars ?? substr($!str, $!pos, 1) !! IterationEnd`,
`count-only` is `chars - $!pos`, and `sink-all` just moves `$!pos`. Every matcher-less form
works this way: `comb`, `comb(Int)`, `comb(Str)`, `lines`, `words` and `split` each have their
own iterator class. `.head(n)` pulls `n` times. `IO::Handle.lines` has a line iterator in the
same shape.

mutsu built an eager `Value::seq` of every piece before `.head` saw any of it. The first
prototype put the string cursor into `LazyList`, next to the `IO::CatHandle` pull (`cat_pull`).
That made `.head` O(prefix). It also showed why ADR-0058 §3.2 rejected `LazyList` as a home for
finite Seqs. Measured against rakudo, the prototype broke:

- `X::Seq::Consumed` after `.head`;
- the `$( )` of an itemized Seq in `.raku`;
- `.Set` (15 elements became 1);
- `~~` against a List.

The reason is that every Seq rule ADR-0034 built lives on `SeqBody`, and `LazyList` has a
parallel set of forcing rules of its own.

## 2. Decision

**A string cursor is a `SeqSource` (`SeqSource::StrIter`), and a `SeqBody` whose source can be
pulled one element at a time serves a consuming `.head(n)` / `.first` by stealing the source and
pulling `n` elements, not by reifying the body.**

1. **The first read settles a `StrIter`.** The source needs no interpreter. So `SeqBody::deref`
   cuts a still-unread `StrIter` into its elements in place (`settle_pure_source`), gated by an
   atomic flag so other Seqs pay one relaxed load. After any read the body is exactly the eager
   Seq mutsu built before. The read path ADR-0058 §7 calls load-bearing can therefore never
   observe an empty seed for this source.
2. **A consuming prefix read steals and pulls a prefix.** `.head(n)`, `.head` and `.first`
   (no matcher) consume a Seq in rakudo. So `SeqBody::take_prefix_source` hands the source over
   (the body becomes `Taken`, as after any `take`), and `take_seq_prefix` pulls `n` elements from
   it. No half-reified state is left behind. This applies when the source is a `StrIter` or an
   `IoLines` read without `kv`, the body has not been read, and `.cache` was not requested.
   Otherwise the call takes the ordinary path.
3. **A bounded subscript pulls a bounded prefix.** `SeqBody::pull_prefix`, formerly the
   `IoLines`-only `pull_io_lines_prefix`, now also cuts a `StrIter`. So `$s.comb[0]` cuts one
   grapheme. The subscript path reads the pulled generation directly, because going through
   `deref` would settle the rest.

## 3. Consequences

- `comb` (no matcher, `Int`, `Str`), `lines`, `words` (method and sub forms, with or without
  `$limit`) and `IO::Handle.lines` / `.words` answer `.head(n)` / `.first` / `[i]` in O(prefix).
  Everything else reads the Seq and costs what it did before. Full consumption was measured
  unchanged.
- `.lines(:count)` counts with the cursor (`StrIterSpec::count_only`) and builds no strings.
- A reader that merely *touches* an unread Seq settles it. For example, `normalize_scalar_assignment_value`
  asked a Seq's length on every `$x = ...`. Such a reader makes the Seq O(n) again, but never
  wrong. `SeqBody::has_unread_str_source` lets a reader skip the touch when all it needs to know
  is that the elements are `Str`s.
- Not covered, and left as follow-ups: `IO::Path.lines` / `.words` (they slurp the file, so
  they are O(file); #9257), `comb(Regex)` (the regex engine finds all matches up front), and a
  count-only `.elems`. For `.elems`, rakudo counts without caching, so a later `.List` then
  consumes. mutsu retains on `.elems` for every Seq, which is a separate, pre-existing
  difference.

## 4. Rejected

- **`LazyList` with a native pull** (§1): it is O(prefix), but each Seq rule has to be rebuilt
  a second time.
- **Partial reification kept across a consuming read**: after `.head` rakudo's Seq is consumed,
  so there is nothing to keep. Stealing the source avoids a new "partly reified, still
  consumable" state in `SeqBody`.
