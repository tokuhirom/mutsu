use Test;

# Multi-candidate RANKING: which candidate a dispatch picks, and how many
# times a `where` constraint is evaluated getting there.
#
# #8696 step 1 changed how the ranking is computed, in two places, without
# intending to change the order it produces:
#
#   - `sort_candidates_by_specificity` (src/runtime/dispatch_resolve.rs)
#     computed each candidate's specificity rank INSIDE a `sort_by`
#     comparator, so a signature walk ran 2*n*log(n) times per sort. It now
#     uses `sort_by_cached_key`, which calls the key closure once per element.
#     The key has to reproduce the old comparator exactly -- rank descending,
#     then the registration stamp, then the registry key string -- or the
#     winner changes for candidates that tie on narrowness.
#   - `choose_best_matching_candidate` (src/runtime/dispatch_candidates.rs)
#     ranked the duplicate registry keys one `multi` is registered under and
#     only then deduped them by body fingerprint; the dedup moved ahead of the
#     ranking loop.
#
# Both are order-preserving by construction, and the failure mode if either is
# wrong is a SILENTLY different winner -- another candidate's body running, or
# an ambiguity going unreported -- rather than an error. So every assertion
# below reads the body that actually ran, and the shapes cover narrowness
# (subset vs its base type vs untyped), narrowness across two positionals,
# arity, and the ambiguity tie set.
#
# All 12 assertions pass under rakudo, including the `where`-evaluation
# count, now that #8697 is fully closed.

plan 12;

# --- Ordering: narrowest candidate wins, across the shapes the ranking
#     reorder could have disturbed.
subset Small of Int where * < 10;
multi narrow(Small $x) { 'small' }
multi narrow(Int $x)   { 'int' }
multi narrow($x)       { 'any' }

is narrow(3),    'small', 'a subset candidate beats its own base type';
is narrow(300),  'int',   'the base type wins when the subset predicate rejects';
is narrow('s'),  'any',   'the untyped candidate wins when no typed one matches';

# --- A narrower positional beats a wider one at the same arity. (A genuine
#     tie cannot be tested for "declaration order wins" here: `$b` already
#     means `Any $b`, so two such candidates are the SAME signature and Rakudo
#     raises X::Multi::Ambiguous rather than picking the first -- which is what
#     the next assertion covers.)
multi narrower(Int $a, Int $b) { 'both-int' }
multi narrower(Int $a, $b)     { 'one-int' }
is narrower(1, 2),   'both-int', 'the candidate narrow on both positionals wins';
is narrower(1, 's'), 'one-int',  'the wider candidate takes the non-matching argument';

# --- Ambiguity is still detected rather than silently resolved.
multi amb(Int $a, $b) { 1 }
multi amb($a, Int $b) { 2 }
throws-like { amb(1, 2) }, X::Multi::Ambiguous,
    'genuinely tied candidates still raise X::Multi::Ambiguous';

# --- Arity narrowing.
multi by-arity($a)         { 1 }
multi by-arity($a, $b)     { 2 }
multi by-arity($a, $b, $c) { 3 }
is by-arity(1), 1, 'arity dispatch: one argument';
is by-arity(1, 2), 2, 'arity dispatch: two arguments';
is by-arity(1, 2, 3), 3, 'arity dispatch: three arguments';

# --- A `where` constraint must be evaluated a BOUNDED number of times per
#     call, not once per registry key the candidate happens to be registered
#     under. One `multi` is registered under several keys (the arity key, the
#     typed key, the `__m<n>` suffixes), and the gathers collect by key, so the
#     same candidate arrives several times over; mutsu drops the repeats by
#     body fingerprint. #7858 established that for RUNNING the constraint;
#     #8696 moved that dedup ahead of the ranking step, so this pins that the
#     guarantee still holds.
my $where-runs = 0;
multi counted(Int $x where { $where-runs++; $x > 0 }) { 'pos' }
multi counted(Int $x) { 'nonpos' }
$where-runs = 0;
is counted(5), 'pos', 'a where-constrained candidate still binds';
# rakudo evaluates the constraint exactly ONCE for a call that matches.
# mutsu evaluated it FOUR times, because one user-level call resolved the
# multi three separate times -- `find_compiled_function_memo`, the
# multi-dispatch frame, and `dispatch_func_call_inner`'s own probe for the
# winner's declaring package -- and for a value-dependent multi (a `where`, a
# subset-typed parameter) `func_multi_dispatch_type_cacheable` refuses the
# resolution cache, so each of those re-ran the user's constraint (#8697).
#
# Two of the three were fixed by reusing the winner the call already
# resolved, leaving 2: one resolution, plus the winning candidate's actual
# parameter bind re-checking a constraint the resolution had already proved.
# The bind now trusts that verdict (`pending_skip_where_recheck`) instead of
# re-running the predicate, so this is at parity with rakudo: exactly 1. This
# also still pins that the per-registry-key dedup #7858 added, which #8696
# step 1 moved ahead of the ranking loop, keeps working -- the count cannot
# climb back toward once-per-registry-key.
is $where-runs, 1,
    'its where clause ran exactly once, at parity with rakudo (#8697)';

$where-runs = 0;
is counted(-5), 'nonpos', 'and the wider candidate wins when the where rejects';
