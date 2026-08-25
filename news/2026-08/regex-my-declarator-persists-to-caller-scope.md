# A regex-declared `:my`/`:constant` lexical now persists to the caller's scope

A regex declarative-prefix `:my $var = ...;` (and `:constant`) is documented
(`raku-doc/doc/Language/regexes.rakudoc`, around the `$paragraph`/`$counter` example) to scope
its variable "within the regex and beyond" — i.e. the variable, and any mutation an embedded
`{ ... }` code block makes to it, is supposed to persist into the caller's enclosing lexical
scope after the match, whether the match succeeded or failed. mutsu instead always discarded it:

```raku
my $s = "abc";
$s ~~ / :my $c = 0; { $c = 1 } /;
say $c;   # raku: 1, mutsu (before this fix): (Any), with "Use of Nil in string context"
```

## Root cause

`regex_match_with_captures` (`src/runtime/regex/regex_match_public.rs`) evaluates a
declarative-prefix declarator (`:my`, `:constant`, `:temp`, `:let`, `:state`) into `self.env`
before running the match, then decides at the end whether to keep or discard each name it wrote.
It bucketed `:my`/`:constant`/`:temp` together into a single `restore_always` map, which — as the
name says — is *always* restored (discarded) once the match returns, regardless of success. Only
`:let`'s bucket (`restore_on_fail`) was ever pushed into `pending_local_updates`/`carrier_writes`
for the caller to see, and only on a successful match.

That conflated three different declarative-prefix semantics that raku actually keeps distinct
(verified against real `raku` for each): `:temp` really is always-restored (regex-local, by
design — it exists specifically to shadow a variable for the duration of the rule); `:let` is
restore-on-fail / persist-on-success (a backtracking-sensitive rebinding); but `:my` and
`:constant` are plain declarations that persist to the caller's scope unconditionally — even
after a match that ultimately fails (`raku -e '"xyz" ~~ / :my $c = 42; a /; say $c'` prints `42`
even though the match itself fails).

## Fix

`:my`/`:constant` now get their own `persist_always` name set, tracked separately from
`restore_always` (`:temp` only now) and `restore_on_fail` (`:let` only). Names in
`persist_always` are never restored, and are unconditionally written back to the caller's local
slots via the existing `pending_local_updates`/`carrier_writes` mechanism (the same one `:let`
already used on success) — both on a successful and on a failed match. A new
`writeback_persist_always` helper centralizes that write-back at every return point in
`regex_match_with_captures` (the two early-parse/eval-failure returns, and the final
matched/unmatched paths).

Verified with `t/regex-my-embedded-block-persist.t` (new, 12 assertions) against real `raku`
output for: a non-quantified embedded block mutating a `:my` var, a plain `*`/`+` quantifier
incrementing a `:my` counter once per iteration, a `:my` initializer surviving an outright failed
match, `:constant` persisting the same way, and confirming `:temp`/`:let` keep their prior
(correct) restore-always / restore-on-fail-persist-on-success semantics unaffected. No regression
in the existing regex `:my`/`:let`/`:temp`/`:state` coverage
(`roast/S05-modifier/my.t`, `t/regex-my-var-interpolation.t`,
`t/regex-my-initializer-and-escaping-sub.t`, `t/regex-inline-code-block.t`,
`t/regex-my-var-in-subpattern.t`, `t/regex-my-lexical-in-make-block.t`,
`t/grammar-dynamic-var-decl.t`, `t/grammar-per-match-dynvar-action.t`, and others).

## What's still open

The original ticket's exact repro (the doc's `*%%`-quantified `$counter` example) still doesn't
match raku's output byte-for-byte after this fix: raku prints `Matched 3 lines`, mutsu now prints
`Matched 17 lines` (previously it printed nothing useful at all, since `$counter` was empty). The
count itself is wrong — a *separate*, deeper bug in how mutsu's backtracking quantifier matchers
eagerly enumerate every candidate length (each running the embedded code block for real) instead
of lazily computing only the lengths an actual backtrack attempt needs, the way raku's engine
does. That is split off into
`todo/deep/regex-quantifier-eager-candidate-enumeration-overruns-code-blocks.md` with a precise
root cause and two possible fix directions, since it is a quantifier-matching architecture change
affecting `src/runtime/regex/regex_match_sep.rs` and the shared
`regex_match_atom_all_with_capture_in_pkg` candidate-enumeration path, not a local fix.
