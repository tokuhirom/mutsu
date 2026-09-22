# Script lookup stops being 161 regexes per character, and the CJK gap goes with it

[The General_Category table](general-category-stops-being-28-regexes-per-character.md)
left one row of its own measurement table unexplained: `.uniprop('Word_Break')`
came down 15.3x but was still 3.2x slower than rakudo, where the other
benchmarks had drawn level or gone ahead. This is that row.

## The gap was CJK, not letters

Splitting the benchmark by which path through `unicode_word_break` a character
takes says it plainly. Release build, same box, mutsu after the
General_Category table:

| case | mutsu | rakudo | |
| --- | ---: | ---: | --- |
| early return (`,`, MidNum) | 6.46 us | 59.11 us | mutsu 9x faster |
| early return (`_`, Pc) | 4.82 us | 7.39 us | mutsu 1.5x faster |
| letter, ASCII `a`..`z` | 3.52 us | 9.02 us | mutsu 2.6x faster |
| **letter, hiragana** | **29.39 us** | **2.84 us** | **mutsu 10x slower** |

Every path was already ahead of rakudo except one, and the aggregate benchmark
had been half hiragana. So there was never a general "Word_Break is slow"
problem to solve — there was a CJK one.

## Why one property cost 8x more for kana than for Latin

`unicode_script_name` was the shape General_Category had just stopped being:
161 compiled `regex::Regex` matches against a one-character string, tried
alphabetically until one hit, then a `String` allocation for a fixed
`&'static str`. `unicode_word_break`'s letter arm calls it **twice** — once
directly, and once through `unicode_line_break(ch) == "SA"`.

The interesting part is that the probe *count* does not explain the 8x.
"Hiragana" is at index 53 of the list and "Latin" at 70, so a kana character
should finish sooner. What actually differs is the cost of each rejection. An
ASCII letter is one byte, and nearly every script class can reject it by
looking at that byte. A hiragana codepoint is `E3 81 82`, and a great many
script classes contain *some* three-byte range starting `E3`, so the UTF-8
automaton has to descend two more levels before it can say no. Each of the
~50 probes before the hit is several times more expensive than the ASCII case.

That is a nice illustration of a rule worth keeping: a linear scan's cost is
the sum of its *misses*, and a miss is not a constant.

## The same three tiers

Script now gets exactly what General_Category got: a 128-byte direct-index
table for ASCII, a two-stage trie over 64-codepoint blocks for the rest of the
BMP, and a binary search over 780 runs above it. **17.8 KB of `.rodata`** —
smaller than the category table's 21.5 KB — with no heap, no lock, no lazy
initialisation. `unicode_script_name` returns `&'static str`.

The generation and verification machinery both properties need now lives once,
in `unicode_table_gen`: the derivation from `regex-syntax`, the tier
construction, the emitter, the drift check, and the exhaustive sweep over all
1,114,112 codepoints. `unicode_gc_gen` moved onto it, which is the only reason
the category data file's header changes in this commit.

Correctness is argued the same way and it is the reason to do it this way: the
table is folded from `regex-syntax`'s Unicode data — the very data `regex`'s
`\p{Script=...}` classes match against — in the order the probe tried them, so
every answer is identical by construction. Two tests hold it there: one
re-derives the tables and drives the lookup over every codepoint, the other
cross-checks against the *actual* 161-regex probe that was deleted.

## Kept deliberately wrong

The script list stays at exactly the 161 names the probe carried. Unicode 15
added `Kawi` and `Nag_Mundari`, which mutsu does not list and therefore answers
`Unknown` for. Adding them is a behaviour change, and this commit's whole
contract is that no answer changes — so they belong in their own commit. (Worth
noting the first draft of the list *did* include them, written from memory;
diffing against the real list caught it. Do not hand-copy a table you can
generate.)

## Measured

<!--MEASUREMENTS-->
