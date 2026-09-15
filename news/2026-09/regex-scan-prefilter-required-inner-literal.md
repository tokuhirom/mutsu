# The regex scan prefilter learns the required inner literal

ADR-0099 Stage 1's third slice ([#8272](https://github.com/tokuhirom/mutsu/issues/8272)),
after the required literal prefix (#8285) and the first-character set (#8446).

The two earlier derivations both answer a question about the **first** character of a
match, so both are silent about `/ \w+ '=>' /`: there is no leading literal, and the
first-character set is as wide as `\w`, which admits essentially every position of an
English-text subject. A 640,000-character failing scan therefore still entered the full
backtracking engine at every one of those positions, at the ~983 instructions per
position ADR-0099 §2.4 measured.

But `'=>'` has to appear *somewhere* in any match of that pattern. One substring search
over the subject answers the entire scan: no occurrence, no match, zero engine entries.

## Numbers

Failing scans over a 588,000-character subject, release build, against
`MUTSU_REGEX_PREFILTER=off`:

| scan | prefilter off | prefilter on |
|---|---:|---:|
| `~~ / \w+ 'zzzq' /` | 1456.9 ms | **10.1 ms** |
| `~~ / .+ 'zzzq' /` | (did not finish in 300 s) | **4.4 ms** |
| `~~ / \d+ '-zzzq-' \d+ /` | (did not finish in 300 s) | **4.3 ms** |

At a 42,000-character subject, where the unfiltered runs do finish, the same three
against rakudo on the same box:

| scan | mutsu before | rakudo | mutsu after |
|---|---:|---:|---:|
| `~~ / \w+ 'zzzq' /` | 108.2 ms | 46.5 ms | **1.4 ms** |
| `~~ / .+ 'zzzq' /` | 262,443 ms | 141,870 ms | **0.3 ms** |
| `~~ / \d+ '-zzzq-' \d+ /` | 13.0 ms | 8.7 ms | **0.2 ms** |

The `.+` row is not a typo: a greedy universal lead-in followed by an absent literal is
quadratic-with-backtracking in both implementations, and it is the shape the inner
literal disposes of most completely. rakudo does not apply this optimization either, so
these are the first rows in the ADR-0099 tables where mutsu's scan is not merely
competitive but in a different complexity class.

## The claim, and why it is narrower than it looks

The derivation yields `(literal, min_before, max_before)` and asserts: every match
contains `literal` at some absolute position `p` with
`start + min_before <= p <= start + max_before`. Contrapositively, a start is viable
only if some occurrence `p` satisfies `p - max_before <= start <= p - min_before` —
which is what the scan iterates, lazily, one occurrence at a time.

That shape is what makes the filter degrade gracefully rather than cliff-edge. With no
occurrence at all the scan is empty. With `max_before` unbounded — the common case,
because every consuming atom other than a plain literal matches a whole grapheme
cluster and so has no upper bound in characters — it still rules out every start beyond
the last occurrence, and the first-character set is applied on top of the surviving
windows, since both are necessary conditions and applying both costs nothing.

A bounded `max_before` does arise, and is worth the arithmetic: `/ 'e'? '=' \d /` turns
each occurrence of `=` into a two-position window instead of "everything up to here".

## Three things worth carrying forward

**This analysis has no engine predicate to duplicate, and that is deliberate.** The
first-character set had to be built by *calling* `regex_match_class_ignorecase` over the
ASCII range, because an independently-written table of what `\w` matches would drift
from the engine's and the drift would be a silently dropped match (ADR-0099 §4
constraint 1). The inner literal sidesteps the problem instead of solving it again: the
only atom it makes a textual claim about is `RegexAtom::Literal`, which the engine
matches with `*ch == c` against a single subject character. Every other atom is opaque
here — it contributes a *length bound* and nothing else. Whoever extends this: adding a
second atom kind that the analysis claims to know the text of is the change that
reintroduces the drift risk.

**Its decline on user code is stronger than the first-set's, and has to be.** A
first-character set only ever skips positions where the leading atom itself fails, so a
`{ … }` block sitting after that atom would not have run there anyway. Skipping a
position because a literal is missing *later* in the pattern skips a block that **would**
have run — so the inner-literal analysis declines on any pattern that can run code
anywhere (ADR-0009: a code assertion runs once per start position in both mutsu and
rakudo). `<subrule>` and `<.ws>` decline with it, both being constraint 3.

**A literal is only required where the concatenation is.** A run inside an optional
group, an alternation branch, or a quantified token is not required at all, and the walk
passes literals up out of a nested group only when that group is matched exactly once.
Getting this backwards is precisely the "wrong without being incorrect" failure the ADR
warns about, so the unit tests pin both directions for each case.

## Testing

The differential corpus (`tests/regex_prefilter_differential.rs`) gains 20 cases, and
the engagement counters (`tests/regex_prefilter_engagement.rs`) four, including one
pinning that a pattern with a code block after the literal is **not** narrowed.
Alongside those, ~5,700 randomly generated pattern/subject pairs — biased toward
inner-literal shapes, checked on `m:g` capture spans and on `.subst`, and run under
`MUTSU_REGEX_PREFILTER=off` for comparison — produced no divergence.

`ScanPositions` and its iterator moved to their own `regex_prefilter_scan.rs` as part of
this, keeping every file under the 500-line limit.

## Adjacent, filed separately

The randomized run did turn up one divergence, and it was not the prefilter's: mutsu
aborts with a spurious `Quantifier range is empty` on
`/ 'x'? \d ** 2 . . 'b' ** 1..3 /`, where no quantifier has an empty range and rakudo
matches. It reproduces with the prefilter on, on subjects where rakudo answers `True`,
so it is a plain engine bug that the randomized comparison happened to surface. Filed as
[#8453](https://github.com/tokuhirom/mutsu/issues/8453).

## Still open on #8272

Subrule-derived prefixes (which have to be keyed by invocant package **and**
`TOKEN_DEFS_GEN` to survive `H is G` overriding `token x`), NFD-aware first-sets for a
*scoped* `:m`, and ADR-0099 §5's NFA over the declarative prefix.
