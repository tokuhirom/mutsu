# A `<+a -b>` composite class narrows a scan instead of sinking it

ADR-0099 Stage 1's sixth slice, and the last atom that answered "anything" rather than answering.

`RegexAtom::CompositeClass` — what `<+alpha -[aeiou]>`, `<[a..z] - [aeiou]>` and `<-[;] - [q]>`
all parse to — widened the whole first-character-set derivation to universal, which makes the
pattern it sits in unfilterable. On a 144,000-character subject a failing `/ <+upper -[A]> ** 3 /`
scan cost the same 205 ms with the prefilter on as with `MUTSU_REGEX_PREFILTER=off`, against 0.8 ms
for a literal scan of that same subject. A cliff, not a shortfall: the same shape written as a plain
character class was narrowed to a bitmap test per position.

Failing scans over a 144,000-character subject, release, against `MUTSU_REGEX_PREFILTER=off`:

| scan | before | after | warm rakudo |
|---|---:|---:|---:|
| `~~ / <+upper -[A]> ** 3 /` | 205.2 ms | **0.6 ms** | 245.7 ms |
| `~~ / <+alpha -[a..z]> ** 3 /` | 76.1 ms | **0.7 ms** | 120.0 ms |
| `~~ m:g/ <+upper -[A]> ** 3 /` | 205.2 ms | **0.8 ms** | — |
| `~~ / <+xdigit -[0]> ** 6 /` | 199.5 ms | 67.4 ms | 497.2 ms |

**Two rows are deliberately not quoted as wins.** `<+xdigit -[0]>` only falls to 67 ms because the
subject really does hold a hex digit every few characters, so most of those engine entries are ones
the prefilter correctly did *not* skip. And `/ <-[;] - [q]> 'zzzq' /` does not move at all
(53–58 ms either way, inside this box's noise): its derived set is every character except two, so
deriving it precisely buys nothing — the same shape `<:!Nd>` showed in the previous slice.

## Why this atom was left for last

A composite class is not a character set. Its `NamedBuiltin` items are matched by the engine as
*two* predicates in sequence: the built-in one (`alpha`, `xdigit`, `upper`, …), and then, when that
rejects, a fallback that resolves a **grammar token** of that name in the invocant package and
matches it against the *remaining input*. The second half is package-dependent,
generation-dependent, and not even single-character — a `<subrule>` in disguise, which is exactly
what ADR-0099 §4 constraint 3 attaches a condition to.

## The two halves are used in opposite directions, and each needed its own justification

The engine's arm is `pos_match && !neg_match`, which is the whole shape of the analysis:

- a **positive** item must be *over*-approximated, because a character it can match has to stay in
  the set. Its built-in half is enumerated over ASCII by calling the engine's own predicate, but its
  grammar-token half cannot be — so a name any rule in the package could answer to declines the
  whole atom back to universal, which is precisely the status quo it had before;
- a **negative** item may *narrow*, and needs no resolution at all. A character it matches on the
  shared predicate is one the arm rejects outright, because the engine runs that same character half
  first and short-circuits on it. The fallback the analysis does not run can only make `neg_match`
  true *more* often — rejecting more characters, never fewer — which is the safe direction.

Getting that asymmetry backwards would have been the silent failure mode: narrowing on a positive
item's built-in half alone drops every match the grammar-token fallback would have found.

## Constraint 1, again by sharing the predicate rather than restating it

The engine's arm held its class-item test as an inline closure. It is a free function now
(`composite_item_matches` in `regex_eval_class.rs`, beside the `class_matches` the plain class arm
already shared for the same reason), along with the `:i` case-fold expansion of the character being
tested (`composite_probe_chars`). The prefilter calls both. Only the grammar-token fallback stayed
at the call site, because it needs the subject and the package — and that half is the one the
analysis declines on rather than re-deriving. A second table of what `<+alpha>` holds would drift
from the engine's, and the drift would be a dropped match rather than an error.

## One condition, one statement of it

Reading the rule registry makes the derivation a fact about `(pkg, TOKEN_DEFS_GEN)` rather than
about the pattern, so such a pattern must reach the package-keyed memo #8464 introduced — and
patterns are shared *by source text* through the regex parse cache, so a registry-dependent set in
the pattern-keyed slot would answer for every package at once. The memo's test
(`mentions_subrule`) and the derivation now consult one shared predicate,
`composite_class_reads_registry`, rather than two readings of "does this class name a rule" that
would be free to disagree. A unit test pins that they agree in both directions.

## Testing

The differential corpus gains twelve composite-class cases (positive built-ins, subtraction,
purely-negated multi-part classes, a non-ASCII subject, a CRLF cluster, `:i`, and a grammar token
shadowing a built-in name), and a randomized sweep of 3,500 generated pattern/subject pairs — half
of them with a user rule shadowing a built-in class name — found no divergence between the
prefilter on and off. `tests/regex_prefilter_engagement.rs` pins the counters in both directions:
that a composite-class scan is sub-linear in subject length, and that a user rule of the same name
puts it back to declining rather than narrowing anyway. `t/regex/regex-composite-class-scan.t`
covers the answers themselves, and passes under rakudo unchanged.

## Adjacent, filed separately

The cross-check against rakudo surfaced a pre-existing `:i` divergence that reproduces with the
prefilter off: mutsu expands the subject character into its case-fold closure before testing a
*cased* built-in name, so `/ :i <+upper -[A]> /` matches a lowercase `z` where rakudo matches only
the `Q`. Filed as [#8498](https://github.com/tokuhirom/mutsu/issues/8498); it is adjacent to
[#8440](https://github.com/tokuhirom/mutsu/issues/8440), the other `:i` question this campaign
turned up. The prefilter follows whichever answer the engine settles on for free, because it calls
the engine's predicate rather than holding its own.

## What remains

ADR-0099 §8 now lists one open item for Stage 1: §5's NFA over the declarative prefix, the
mechanism rakudo uses, and the thing that would subsume the six ad-hoc derivations with one.
