# General_Category stops being 28 regexes per character

`unicode_general_category(ch)` answered "which General_Category is this
character in?" by running up to 28 compiled `regex::Regex` matches against a
one-character string, in a fixed order, until one hit — and then allocating a
`String` for an answer that is a fixed `&'static str`. Most callers
immediately did `matches!(gc.as_str(), "Lu" | "Ll" | ...)` on it and dropped
the allocation.

It cost ~970 instructions per call, and the call is per *character*:
`nqp::iscclass` / `findcclass` / `findnotcclass` (the inner loop of a
hand-rolled NQP scanner such as `JSON::Fast`'s `parse-string`), `.uniprop`,
`unimatch`, collation, and the UAX #29 segmentation properties all reach it.
A `JSON::Fast` decode paid it 235,200 times across ten decodes — 0.89% of the
whole program and 23,520 allocations per decode. The benchmark in
[#8999](https://github.com/tokuhirom/mutsu/issues/8999) was a *favourable*
case: `a`..`z` are `Ll`, the second regex tried. A digit is the ninth,
ordinary punctuation the eighteenth, a space the twenty-third.

## A table, generated from the same Unicode data

`src/builtins/unicode_gc.rs` now answers it from static tables, in three tiers
picked so the common case is the cheapest:

| codepoint | structure | cost |
| --- | --- | --- |
| `< 0x80` | direct index into a 128-byte table | one load |
| `< 0x10000` | two-stage trie, 64-codepoint blocks | two loads |
| above | binary search over 1,200 runs | ~11 branches |

Almost all the text mutsu classifies is ASCII, so it gets its own table rather
than paying the trie's second dependent load; the BMP tier covers CJK, kana
and every other everyday script at O(1); only astral codepoints binary-search.
Total static data is ~21 KB in `.rodata` — no heap, no lock, no lazy
initialisation, nothing memoized per process or per thread.

The result is a `GeneralCategory` enum rather than a `String`. Callers that
want the abbreviation take `as_str()` (a `&'static str`); callers asking group
questions take a bitmask, so `matches!(gc, "Lu" | "Ll" | "Lt" | "Lm" | "Lo")`
becomes one `&`.

**The tables are generated from `regex-syntax`'s Unicode data — the very
tables `regex`'s `\p{...}` classes match against — folded in the same priority
order the ordered probe used.** That is what makes the answers identical to
the regexes' by construction rather than by argument, and it is why this
needed no new Unicode table and no new correctness case. `unicode_gc_gen`
re-derives them on every `cargo test` run and fails if the committed file has
drifted, so a `regex-syntax` bump carrying a new Unicode version cannot land
silently. It checks all 1,114,112 codepoints, and a second test cross-checks
the tables against the *actual* ordered `regex` probe that was removed.

## `is_cclass` answers thirteen questions with one load

`nqp::iscclass`'s thirteen `CCLASS_*` members were thirteen string comparisons
against the category name, every one of them evaluated even after a match. All
thirteen are now bits in a `const` table indexed by the category, plus four
codepoint tests for the members that are not a function of the category
(ASCII hex digits, the line breaks, tab, and underscore). One table load and
an `&` replace the lot.

## And a regex compiled once per character

While measuring, `check_binary_property` turned out to call
`regex::Regex::new()` — compiling a pattern from scratch — on **every call**,
and it is called per character (several times per character from the
segmentation properties). That is not a 28-probe problem, it is a ~300µs one.
The compiled matchers are now cached thread-locally, keyed by the pattern
literal, the way `runtime::unicode`'s identical cache already worked. The two
call sites in `runtime::unicode` that stringified a `char` per match now
`encode_utf8` into a stack buffer instead.

## Measured

Release build, paired A/B against `main` minutes apart, first run of each
discarded per the warm/cold rule in `.agents/skills/perf-tuning/SKILL.md`:

| benchmark | before | after | rakudo | speedup |
| --- | ---: | ---: | ---: | ---: |
| `findnotcclass` over 4 M characters | 0.2753 s | **0.0172 s** | 0.0212 s | **16.0x** |
| `.uniprop` x 20,800, mixed ASCII/CJK/punctuation | 0.0478 s | **0.0340 s** | — | 1.4x |
| `.unimatch('Alphabetic')` x 1,140 | 0.3388 s | **0.0036 s** | 0.0041 s | **94x** |
| `.uniprop('Word_Break')` x 1,140 | 0.2416 s | **0.0158 s** | 0.0049 s | **15.3x** |

Three runs of each after a discarded warm-up; the `before` column is the
`main` binary measured on the same box minutes earlier, the same way. The
rakudo column is the reference implementation on the same box.

Two of those now run *faster than rakudo*. `findnotcclass` was **10.7x slower
than rakudo** in the issue's measurement and is now slightly faster than it;
`.unimatch('Alphabetic')` was 83x slower and is now slightly faster.
`.uniprop('Word_Break')` went from 49x slower to 3.2x, and what remains there
is `unicode_script_name` and the segmentation properties' own `String`
returns, not General_Category.

The `.uniprop` row is the modest one and worth saying so: at 20,800 calls it
is dominated by method dispatch and `Value` construction, so removing ~970
instructions per call moves it 1.4x, not 16x. The scanning benchmark is the
one where General_Category was the whole cost.

## Correction (2026-09-22): the `Word_Break` rakudo ratio above is overstated

The `.uniprop('Word_Break')` row says 49x slower than rakudo before and 3.2x
after. The before/after ratio holds, but **both absolute figures include the
one-time lazy compilation of `unicode_script_name`'s 161 regexes**, amortized
over only 1,140 calls. That compilation costs 5.52 ms, or 4.8 us of every one
of those 1,140 calls, so the "3.2x slower than rakudo" comparison measures
mutsu's start-up against rakudo's steady state.

Re-measured one case per process at n=5,200 after a warm-up, the commit this
entry describes left mutsu at **2.973 us** per ASCII letter and **3.127 us**
per hiragana against rakudo's 3.063 us and 3.116 us -- that is parity, not
3.2x. The same mistake is written up at length, with the two rules that
prevent it, in
[the Script table entry](script-lookup-stops-being-161-regexes-per-character.md).

The other three rows are not affected: `findnotcclass` runs 4 M characters and
`.uniprop` 20,800, so a 5.52 ms constant is noise in both, and
`.unimatch('Alphabetic')`'s cost was a `Regex::new` *per call*, not once.

## Not done here

`unicode_script_name` has the same shape the General_Category probe had —
over 150 regexes tried in order, returning a `String` — and is reached per
character from the line-breaking property. It wants the same treatment and is
a separate change.
