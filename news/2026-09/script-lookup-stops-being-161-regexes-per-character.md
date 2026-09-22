# Script lookup stops being 161 regexes per character

`unicode_script_name(ch)` was the shape
[General_Category had just stopped being](general-category-stops-being-28-regexes-per-character.md):
161 compiled `regex::Regex` matches against a one-character string, tried
alphabetically until one hit, then a `String` allocation for an answer that is
a fixed `&'static str`. It is reached per character, and twice per character
from `unicode_word_break`'s letter arm — once directly and once through
`unicode_line_break(ch) == "SA"`.

Script now gets exactly what General_Category got: a 128-byte direct-index
table for ASCII, a two-stage trie over 64-codepoint blocks for the rest of the
BMP, and a binary search over 780 runs above it. **17.8 KB of `.rodata`** —
smaller than the category table's 21.5 KB — with no heap, no lock and no lazy
initialisation. `unicode_script_name` returns `&'static str`.

The generation and verification machinery both properties need now lives once,
in `unicode_table_gen`: the derivation from `regex-syntax`, the tier
construction, the emitter, the drift check, and the exhaustive sweep over all
1,114,112 codepoints. `unicode_gc_gen` moved onto it, which is the only reason
the category data file's header changes here.

Correctness is argued exactly as it was for General_Category, and that is the
reason to do it this way: the table is folded from `regex-syntax`'s Unicode
data — the very data `regex`'s `\p{Script=...}` classes match against — in the
order the probe tried them, so every answer is identical by construction. One
test re-derives the tables and drives the lookup over every codepoint; another
cross-checks against the *actual* 161-regex probe that was deleted.

## Measured

`.uniprop('Word_Break')` per character, one case per process, 5,200 calls after
a warm-up, three runs; median quoted. Baseline re-built from the `main` this
branches off, per `.agents/skills/perf-tuning/SKILL.md` §0.

| case | before | after | rakudo | speedup |
| --- | ---: | ---: | ---: | ---: |
| letter, ASCII `a`..`z` | 2.973 us | **2.061 us** | 3.063 us | 1.44x |
| letter, hiragana | 3.127 us | **2.118 us** | 3.116 us | 1.48x |

mutsu was already at rough parity with rakudo on this path and is now about
1.45x ahead of it on both.

Separately, and not visible in a steady-state number: the `OnceLock` that built
those 161 matchers cost **5.52 ms** the first time any script lookup happened in
a process. mutsu is a CLI, so a one-off 5.5 ms on a short script is not nothing.
The table has no initialisation at all.

## The measurement mistake that nearly became the headline

The first draft of this entry led with a much better story: that the remaining
gap after the General_Category work was **CJK-specific**, hiragana costing
29.39 us against rakudo's 2.84 us while ASCII was already ahead — and it
explained why, at length and plausibly, in terms of UTF-8 automaton descent
(an ASCII byte is rejected by most script classes immediately, whereas `E3 81
82` shares its lead byte with many three-byte ranges).

None of it was real. The 29.39 us came from a benchmark that ran four cases in
one process at n=520, iterating a hash — so the case that happened to run first
absorbed the one-time lazy compilation of those 161 regexes. At 5.52 ms over
520 characters that is **10.6 us/char** of pure start-up, and hiragana drew the
short straw on that run. Re-measured one case per process at n=5,200, `main`
answers 3.127 us for hiragana against 2.973 us for ASCII: no CJK gap, and no
10x anything.

The mechanism story was coherent, quantitatively plausible, and wrong, which is
the combination worth being afraid of. Two rules earned their keep and are worth
restating:

- **Amortize start-up over a big enough n, or measure it separately.** A lazy
  `OnceLock` is invisible in the code at the call site and enormous at n=520.
- **One case per process.** Sharing a process let the first case pay for the
  rest, and hash iteration order decided which one that was — so the same
  benchmark told a different story run to run.

The change is still worth making; it is just worth 1.45x and a 5.5 ms
start-up, not 10x.
