# A `multi` sub-signature that destructures a hash by name now matches

`multi sub patch(\c, %patch (:op($)! where { $_ eq 'add' }, :@path!, :$value!))` is how
`Crane::Patch` writes all six of its operation candidates. Under mutsu every one of them failed
to match, so every `Crane.patch` call fell through to the dist's `default` CATCH arm and came back
as `Crane accident: patch operation failed`. The identical signature written as a plain `sub`
bound fine — which is the tell, because a non-`multi` call goes straight to the binder and never
consults the multi-candidate matcher.

## Root cause

An `Associative` argument's capture has **no positional part**. Rakudo's
`{:x(1), :y(2)}.Capture.list` is `()`; the entries live in its `.hash`. So a sub-signature that
destructures a hash by name (`%h (:$x!, :$y!)`) consumes no positional slot at all, and its
entries have to be checked as **named** arguments.

`sub_signature_matches_value` instead read the unpack target through
`positional_values_from_unpack_target`, which reports one element per hash entry — the shape the
*positional* destructure forms (`@a ($first, $second)`) need. An all-named sub-signature names none
of those synthetic positionals, so the leftover-positional check saw every entry unconsumed and
rejected the candidate.

The `Pair` case one level down had already been special-cased for exactly this reason
(`destructures_pair_by_name`): a `Pair`'s capture has no positional part either. The hash case is
the same argument applied to the container that holds many of them.

## The fix

Two predicates in `src/runtime/types/signature.rs`, mirroring the existing `Pair` pair:

- `destructures_associative_by_name` recognises an all-named (or slurpy-only) sub-signature against
  a `Hash`/`Map` unpack target, and makes the leftover-positional check sit that case out.
- `associative_entries_all_named` then checks the entries the way a call checks named arguments: an
  entry no parameter names is an unaccounted named argument and the candidate does not match, which
  is what rakudo does with `%h (:$x!)` against `{:x(1), :y(2)}`. A **named** slurpy (`*%rest`)
  accepts the remainder; a **positional** slurpy (`*@rest`) does not, since there are no positional
  parts for it to take.

`named_param_addresses_key` resolves which parameter consumes an entry, handling both the `:$op`
spelling (the name sits in `pd.name` with its sigil) and the `:op($o)` rename form (where `pd.name`
is the *source* name and the target lives in its own sub-signature).

## Pin

`t/routines/signature/multi-subsignature-destructures-an-associative.t` — 17 tests, green under
mutsu and under real Rakudo. It covers the matching side (`%h`, `$c` and `Map` spellings, the
three-candidate `where` shape `Crane::Patch` actually uses), the refusal side (an unnamed entry, a
missing required named, a positional slurpy that cannot account for one), and the neighbouring
shapes that must not have moved (a positional destructure of an `Array`, a `Pair` destructured by
its named parts, an all-named sub-signature against a plain `Array`, and the plain `sub` spelling
that never had the bug).
