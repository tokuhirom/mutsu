# A regex scan rejects a start position without entering the engine

[#8248](https://github.com/tokuhirom/mutsu/issues/8248) recorded the one shape
where mutsu lost outright to rakudo: a failing regex scan over a large subject.
Both interpreters are linear in subject length, but mutsu's constant per
*failing candidate position* was roughly an order of magnitude larger, so the
ratio degraded monotonically with size and crossed 1.0 somewhere around half a
megabyte. A failing or mostly-failing scan over a big buffer is what real code
does to a log file, a source file or a fetched document, so that constant is the
number that decides whether mutsu can process real-sized input at all.

ADR-0099 Stage 1 had already landed the first half of the answer: a pattern
whose body begins with a plain literal run gets a substring search instead of a
walk. What was left was every pattern with *no* such prefix — an alternation, a
`:i` literal, a character class, a `\w+` run — which still entered the full
backtracking engine at every character position, ~983 instructions to establish
one rejection (ADR-0099 §2.4).

This adds the second derived fact the ADR names: the set of characters a match
can **begin** with. A position whose character is outside that set cannot start
a match, so it is rejected by one bit test rather than by an engine entry.
`'zzq' | 'yyq' | 'xxq' | 'wwq'` yields `{w,x,y,z}`; `:i 'ZZZQ'` yields `{Z,z}`;
`\d+` yields the ten ASCII digits.

## What it is worth

Release build, 640 KB subject of repeated 46-character units, one scan per row,
against the same box's rakudo 2026.07:

| shape | before | after | rakudo |
|---|---:|---:|---:|
| failing four-literal alternation | 1,133 ms | **124 ms** | 801 ms |
| failing `:i` literal scan | 246 ms | **16 ms** | 9 ms |
| failing `<[qQ]> 'ZZ'` class scan | 171 ms | **15 ms** | 118 ms |
| `'67-8' \s $` end anchor | 177 ms | **28 ms** | 6 ms |
| `.comb(/ \d+ /)` over the whole subject | 127 ms | **48 ms** | 461 ms |
| failing `\w+ 'QQQ'` | 716 ms | 619 ms | 340 ms |

The issue's headline row inverts: the alternation scan was 1.41x slower than
rakudo and is now 6.5x faster. The `:i` row closes a 27x gap to 1.7x. The
end-anchor row moved for a reason worth recording on its own — a *quoted*
literal (`'67-8'`) parses into a `Group`, which the existing literal-prefix
analysis walks straight past, so that whole family of patterns had been
declining the prefilter without anyone noticing; the first-set analysis looks
through a group and catches them.

`\w+ 'QQQ'` is the row that barely moves, and correctly so: `\w` admits about
85% of this subject, so there is almost nothing for a first-character set to
reject. What is left there is the per-position cost itself, which is a
different piece of work.

Across the regex benchmark suite (three runs each, median), nothing regressed:
`bench-regex-split-subst` 378 → 194 ms, `bench-regex-long-subject` 545 → 254 ms,
`bench-regex-global` 253 → 188 ms, `bench-regex-match` 278 → 243 ms,
`bench-grammar-parse-big` unchanged (a `Grammar.parse` is anchored, so no scan
is involved — exactly what the ADR predicted).

## Staying sound

The only way a prefilter can be *wrong without being incorrect* is by being too
narrow: a set that omits a character some match really could start with silently
drops that match, and no ordinary `.t` file catches it, because the unfiltered
engine would answer the same way. So every rule here is an over-approximation,
and anything not provably over-approximating declines outright, restoring the
unfiltered scan. Three of those over-approximations are load-bearing:

- **Character classes are probed, not re-derived.** The set is built by asking
  the matcher's own predicate about every character in `0..=0xFF`, so the
  analysis and the matcher cannot drift the way a second reading of `ClassItem`
  would. `regex_match_class`'s body was lifted to a free `char_class_matches`
  for this and the method now delegates, so there is still exactly one
  definition. Anything a class might match above U+00FF is covered by a `wide`
  escape-hatch bit. This is also why `<alpha>` comes out right without a special
  case: Raku's `<alpha>` is `<+alpha +[_]>`, and probing knows that because the
  matcher does.
- **`\r` is admitted whenever `\n` is**, because the `CharClass` atom matches a
  `\r\n` pair as the single grapheme `\n`, so such a match really does start at
  the `\r`.
- **`:i` always sets `wide`.** U+212A KELVIN SIGN lowercases to `k`, so an ASCII
  `:i` literal is reachable from outside Latin-1.

A nullable leading token, a separator quantifier, `:m`, an assertion, a subrule,
`.` and a runtime-interpolated literal all decline. So does a set dense enough
to reject almost nothing (a negated class), so the bit test is never paid for
nothing.

The gate is the differential corpus in `tests/regex_prefilter_differential.rs`:
every case runs twice, once normally and once with `MUTSU_REGEX_PREFILTER=off`,
and the two outputs must be byte-identical. It grew the long-subject half it was
missing, including a match whose only occurrence starts outside Latin-1 and an
`:i` match that begins at a KELVIN SIGN — the two cases a narrower set would
drop.

## The memoization that the first cut got wrong

The analysis was first derived inside `regex_scan_positions`, behind a
minimum-remaining-positions threshold so it would amortize. That reasons about
the wrong loop. `.comb`, `:g`, `split` and `.match(:g)` *restart* the scan after
every match, and each restart still has a long tail of subject left, so the
threshold let the whole analysis be charged once per **match**: `$big.comb(/ \d+
/)` over the 640 KB subject (54,612 matches) went from 136 ms with the prefilter
off to 208 ms with it on. The filter cost more than the engine entries it saved.

The set is now memoized on the `RegexPattern`, next to the existing
`stripped_pattern` memo and on the same `OnceLock` shape — which is what
ADR-0099 §4 asked for in the first place. It is derived once per parsed pattern,
the parse cache already shares those, and the threshold is gone, so short
subjects benefit too rather than being excluded to dodge a cost that no longer
exists. `FirstCharSet`'s Latin-1 half is a 256-bit set, so membership is a shift
and a mask, a union is four `|`s, and the memo costs 40 bytes per pattern.

## One adjacent waste, on the same workload

`needs_casefold_expansion` asks `has_multichar_fold` about every character of
the subject on every `:i` match, and that allocated a `Vec` and two `String`s
per character — 655,344 of each on the 640 KB `:i` scan. ASCII case mapping is
1:1 in both directions, so the overwhelmingly common answer is now one range
check.
