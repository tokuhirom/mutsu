# Multi dispatch stops at the first candidate that binds

`Config::TOML` v0.1.3's `t/special-cases/03-txn.rakutest` parses a single
350-line TOML ledger. It produced the right answer but took **69.8s** where
`raku` took 2.0s on the same box ([#7858](https://github.com/tokuhirom/mutsu/issues/7858)).
It now takes **16.9s** — 4.1x faster — and the dist's whole suite went from 91s
to 31s with an identical pass/fail set.

## What was slow

The issue named three suspects; measurement ruled out two of them straight
away. Timing `Config::TOML::Parser::Grammar.parse` with **no** `:actions`
showed the grammar is linear and cheap (0.58s for the four-entry input, the
whole of which takes 26s with actions), and truncating the input to 1..7
ledger entries produced a **linear** curve, not the quadratic one a
document-copying `Crane.set` would give. What was left was a large constant
factor per keypair, and a `Crane.exists` microbenchmark isolated it: 200 calls
cost mutsu 2.09s against rakudo's 0.008s.

`Crane`'s API is built out of `where`-constrained protos — `in` has 17
candidates, `exists-key` 7, and their constraints recurse back into `Crane`
(`@path where { .elems > 1 and exists-key($container, [@path[0]]) }`). A
callgrind run put `choose_best_matching_candidate` at 64% of the whole program,
which is the shape the issue predicted.

## Three fixes, in the order they matter

**Dispatch walks narrowest-first and stops.** `choose_best_matching_candidate`
used to bind-test *every* candidate and only then rank the ones that matched.
But `candidate_rank_key` reads nothing but the declared signature and the
argument types — it never runs user code, and in particular never evaluates a
`where` clause (it only counts how many params carry one). So the whole
candidate list can be ordered up front, and the bind attempts can stop as soon
as the remaining candidates are strictly wider than the best match: a wider
candidate can neither win nor join the tie set that decides
`X::Multi::Ambiguous`, so testing it cannot change the answer. Raku dispatches
this way too, and the difference is observable, because a `where` clause is
user code:

```raku
my @log;
multi sub f(Int:D $x where { @log.push('int'); True }) { 'int' }
multi sub f($x       where { @log.push('any'); True }) { 'any' }
f(1);   # rakudo: @log is ['int']. mutsu used to run 'any' too.
```

A wider candidate whose `where` *dies* is likewise never reached now, which is
what rakudo does and what the `where`-throws rule in
[#7539](https://github.com/tokuhirom/mutsu/issues/7539) was approximating by
comparing ranks after the fact. Because the scan breaks at the first strictly
wider candidate, every collected match ties with `matches[0]` on all five
narrowness components by construction — so the post-scan re-sort and the `tied`
re-filter, which recomputed `candidate_specificity_rank_for_args` and
`candidate_type_distance` per match, are gone too.

**Candidates are deduplicated before they are tested, not after.** One `multi`
is registered under several registry keys (the arity key `Pkg::f/1`, the typed
key `Pkg::f/1:Int`, the `__m<n>` multi suffixes) and the gathers collect by key,
so the same `Arc<FunctionDef>` arrived two or three times over. The `seen`
fingerprint filter ran *after* matching, so each copy's `where` clause was
actually run. Moving the filter ahead of the bind test cut a two-candidate
`multi`'s constraint evaluations from 10 per call to 4.

**The bind snapshot no longer copies the whole env.** `args_match_param_types_inner`
cloned the flat env and wrote the parameter bindings into it, which
`make_mut`-deep-copied every entry on the first write; the rollback
(`restore_env_preserving_dynamics`) then walked every entry again looking for
dynamic-variable writes, resolving each key to a `&str` through a thread-local
to test its sigils. Binding into a `Env::scoped_child` overlay instead — the
same treatment `method_args_match_for_invocant` already had — makes the bind
O(binds) and leaves the rollback walking only what the match wrote, and the
dynamic-name test now reads the memoized `DYNAMIC_VAR_ENV_KEY` symbol flag.
That flag mirrors `is_dynamic_var_env_key`, which trims *every* sigil before
looking for the `*` twigil, so `@*x` / `%*x` / `&*x` writes from a `where`
clause now survive the rollback alongside `$*x`; the older hand-rolled test
(`*` or `$*` prefix) simply missed them.

## Measured

Release builds, same box, `main` at 69bedb4 against the branch:

| | main | after | raku |
| --- | --- | --- | --- |
| `Crane.exists` x200 | 2.093s | 0.408s | 0.008s |
| `Crane.set` x200 | 1.737s | 0.405s | 0.035s |
| `03-txn.rakutest` | 69.8s | 16.9s | 2.0s |
| whole `Config-TOML` suite | 91s | 31s | — |

## What is still slow

The gap to rakudo on that file is now ~8x rather than ~35x, and the profile has
moved off dispatch. Two findings from the same callgrind run are filed
separately rather than stretched into this change: `dispatch_func_call_inner`
resolves the same call **three** times (`find_compiled_function_memo`, then
`resolve_function_multi_cached`, then `push_multi_dispatch_frame`), which is
why a `where` constraint still runs 4x per call where rakudo runs it once; and
`eval_block_value_inner` clones the entire registry `functions` map on every
block evaluation to be able to restore it, even though the restore itself is
already skipped when `registry_write_gen` says nothing was declared.
