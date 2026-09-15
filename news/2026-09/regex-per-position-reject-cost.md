# The per-position reject cost: three things the scan was paying for and never reading

[#8450](https://github.com/tokuhirom/mutsu/issues/8450), the residue left by ADR-0099 Stage 1. The
scan prefilter ([#8272](https://github.com/tokuhirom/mutsu/issues/8272),
[#8285](https://github.com/tokuhirom/mutsu/issues/8285)) answers "can a match start here?" without
entering the engine, and it turned every shape in #8248's table into a mutsu win — except one.
A pattern whose first-character set is *dense* has nothing to reject: `\w` admits about 85% of an
ordinary text subject, so almost every position still enters the engine, and what `/ \w+ 'QQQ' /`
measures is the raw per-position reject cost. On a 640 KB subject that was 655 ms against rakudo's
322 ms.

The ticket named two hypotheses to rule out first — the greedy give-back re-entering its
continuation once per surrendered character, and a `CapStore` built per start position. Callgrind
on the same shape (40 KB subject, 304.9 M instructions) said neither was the largest item. Three
other things were, and all three were work whose result nothing read.

## A grapheme image built for every character tested against every class

The `CharClass` atom built the NFC image of the grapheme at the position it was asked about —
`chars[pos..ge].nfc().collect::<String>()` — before testing membership, and then handed that string
to a comparison that only a `ClassItem::Grapheme` entry can consume. A class *without* such an
entry, which is `\w`, `\d`, `\s`, `<[a..z]>` and very nearly every class anyone writes, had its
`any()` return `false` for every item without ever looking at the string. So the atom paid a
`malloc`, an NFC pass and a `free` per character tested, for a value with no reader: 91,269 calls,
**12.3%** of the run.

Hoisting the "does this class hold a grapheme entry at all?" test out of the loop is an exact
transformation — when it holds none, the old `any()` was already `false` — and the string is now
built only on the path that reads it.

## Two grapheme helpers establishing that ASCII has no combining marks

`grapheme_end` (214,099 calls) and `is_grapheme_boundary` (152,684) between them called
`unicode_normalization::is_combining_mark` 519,465 times, **8.6%** of the run, over a subject that
is entirely ASCII. No ASCII codepoint is a combining mark, so a non-control ASCII base followed by
ASCII is a cluster of its own, and an ASCII codepoint preceded by an ASCII one always starts one.
Both now short-circuit on that and fall through to the general path otherwise, which keeps CRLF and
UAX #29 GB4 exactly as they were — controls are excluded from the fast path precisely so `\r` and
`\t` still reach the rules that know about them.

## A pair of fresh `Vec`s per scan position

`walk_quant_chain` records one end position and one `CapStore` mark per iteration it grows, in two
`vec![]`s. It is entered once per scan position, so growing those two chains was **10.7%** of what
remained after the first two fixes — `RawVec::grow_one` alone, 63,542 calls. They come from a pool
on the interpreter now, in the shape `args_scratch_pool` already established: the walk is
recursive, so the pool holds one buffer per nesting level in flight, bounded, cleared on return.

## Measured

Release, 640 KB subject of repeated 46-character units, one scan per row, against rakudo 2026.07 on
the same box. Best of three:

| shape | before | after | rakudo |
|---|---:|---:|---:|
| failing `\w+ 'QQQ'` | 655 ms | **404 ms** | 322 ms |
| failing `:r \w+ 'QQQ'` | 335 ms | **175 ms** | 479 ms |
| failing `[ \w+ \s ] ** 3 'QQQ'` | 2,883 ms | **1,865 ms** | 1,218 ms |
| `.comb(/ \d+ /)` over the subject | 46 ms | **32 ms** | 288 ms |
| `'67-8' \s $` end anchor | 28 ms | **25 ms** | 5 ms |
| failing four-literal alternation | 123 ms | 120 ms | 605 ms |

The ticket's headline row goes from 2.0x slower than rakudo to 1.25x. The ratchet row, already a
win, roughly doubles it. Callgrind over the whole 40 KB scan: 304.9 M → 201.7 M instructions,
**-34%**, with the allocator off the profile's top list entirely.

The regex benchmark suite, best of three, moves with it and nothing regresses:
`bench-regex-long-subject` 248 → 172 ms, `bench-regex-assertion` 181 → 136 ms,
`bench-regex-match` 206 → 172 ms, `bench-regex-global` 194 → 172 ms,
`bench-regex-capture` 394 → 331 ms, `bench-regex-split-subst` 156 → 133 ms,
`bench-grammar-parse-big` 79 → 72 ms.

## What is left, and what it is not

None of this is the structural change ADR-0099 §4 Stage 2 contemplates. What the profile shows now
is the walk itself: `walk_quant_chain` → `grow_one_iter` →
`regex_match_atom_with_capture_in_pkg` → `for_each_atom_candidate` →
`regex_match_atom_in_pkg_inner`, five layers deep, each moving a `RegexCaptures` by value, for an
atom that captures nothing. That is the compiled-form question, and it wants its own decision
rather than another round of shaving — so #8450 closes on the measurement it asked for, not on
parity.

Both hypotheses the ticket listed are answered, and both are smaller than they looked: the
give-back is real but its cost is dominated by what each re-entry *did* per character (the three
items above), which is why the ratchet row — one grow, no give-back — improved by nearly the same
proportion. The per-start-position `CapStore::new` / `RegexCaptures::default` shows up as
`regex_walk_ends_in_pkg`'s own 1.5% — about 101 instructions for each of the 29,854 positions —
worth having eventually, but not the answer either.

Pinned by `t/regex/regex-charclass-grapheme-fastpath.t`, whose expectations were all read off
rakudo: a class holding a multi-codepoint cluster still matches it, and the classes that now take
the short path still refuse to match the base of a synthetic grapheme or to start on a mark inside
one.
