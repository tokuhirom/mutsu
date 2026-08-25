# Native builtin methods honour the implicit `*%_`

Every Raku *method* carries an implicit `*%_` slurpy named parameter, so a named
argument the method does not declare is silently swallowed. mutsu honoured that
for user-defined methods but not for native builtin ones: `4.log(:base(2))` and
`"abc".uc(:foo)` died with `X::Method::NotFound` (complete with a misleading
"Did you mean 'log2'?" / "Did you mean 'fc'?" suggestion) where Rakudo returns
`4.log` and `"ABC"`. Subs have no such implicit parameter and must keep
rejecting an unexpected named — `sub s() { 42 }; s(:foo)` dies in both
implementations — so the fix had to apply to method dispatch only.

## Root cause

The ticket's file pointer was stale: `call_method_with_values` lives in
`src/runtime/methods_call_dispatch.rs`, not `src/runtime/methods.rs`. What it
described was right, though, and generalizes beyond the arity cascade:

> **a named argument was allowed to occupy a positional slot.**

Two symptoms fall out of that one cause.

1. **Arity dispatch missed.** Native methods are selected by arity
   (`native_method_0arg` / `_1arg` / `_2arg`), and the named `Pair` was counted
   as a positional. `4.log(:base(2))` looked up a 1-ary `log`, found nothing,
   and raised "no such method" — even though a 0-ary `log` was sitting right
   there. Measured across a 57-probe survey against `raku`, this accounted for
   24 divergences (`log`, `uc`, `chars`, `round`, `join`, `reverse`, `head`,
   `List`, `keys`, `values`, `list`, `Int`, `base`, `flip`, `words`, `lines`,
   `pick`, `ords`, `Bag`, `kv`, `batch`, `roll`, `combinations`, `total`).

2. **Arity dispatch hit the wrong arm, silently.** When a slot happened to
   accept the `Pair`, it was numified or consumed as data instead. That is worse
   than an error: `"a,b,,c".split(",", :nonsense)` read `:nonsense` as `$limit`,
   numified it to 0, and returned an *empty* `Seq`; `"abc".comb(:nonsense)`
   read it as the matcher and regex-searched for `":nonsense"`;
   `(1,2,3).max(:zzz)` returned the pair itself.

## The design, and why

The tempting fix — strip named arguments before dispatch — is wrong: a native
method that genuinely reads an adverb (`.split(:skip-empty)`,
`.substr-eq(:i)`, `.comb(:match)`, `.min(:by)`, `.first(:k)`, `.subst(:g)`, …)
reads it out of that very argument list, and the set of names each one consumes
is implicit in its Rust body rather than declared anywhere. Stripping up front
would silently drop real adverbs. Reversing the order (positionals first, full
list as fallback) has the same flaw: `"a,b,,c".split(",", :skip-empty)` would
hit the positional-only `split(",")` and lose `:skip-empty`.

So the mechanism chosen is the one that **cannot change any call that works
today**: `call_method_with_values` became a thin wrapper around the original
body (now `call_method_with_values_inner`). It offers the *full* argument list
first — byte-for-byte today's behaviour — and only when the dispatch chain
reports `X::Method::NotFound` *for this very method* (meaning nothing in the
chain understood this argument list at all) does it retry with the named
arguments removed. Named-ness is read off the value flavour, which ADR-0021
already made a faithful call-site property: only `ValueRepr::Pair` is a named
argument, so a positional `Pair` (`%h.push((a => 1))`, a `Pair` held in a
variable) is untouched. A wrong-arity *positional* call carries no nameds at
all, so `"abc".uc("x")` and `4.log(1,2,3)` never take the retry path and still
die.

The retry re-enters the whole dispatch chain, which exposed a hazard worth
recording: a `Seq` body is single-use, and `call_method_with_values` consumes it
(`reify_or_consume_seq_target_authoritative`) for every `seq_method_consumes`
entry — `join`, `keys`, `kv`, `head`, `Bag`, `List`, … i.e. precisely the
methods this fix rescues. Retrying naively made the second attempt steal an
already-`Taken` body and throw `X::Seq::Consumed` on what is, to user code, the
first call. The wrapper therefore performs that single touch itself when the
receiver is a `Seq`, and passes a `reify_seq: false` flag so neither attempt
touches it again. `t/native-method-implicit-slurpy-named.t` pins this with
`(1,2,3).map({ $_ * 2 }).join("-", :foo)`.

Symptom 2 is not reachable by that mechanism — those calls *succeed*, wrongly,
so there is no error to retry on. Where the affected code already carries an
explicit list of the names it accepts, the local fix is unambiguous and was
applied: an unrecognized named-flavour `Pair` is dropped rather than pushed into
the positional vector, in `SplitOpts::from_args` (`src/builtins/split.rs`),
`native_comb_method` (`src/builtins/comb.rs`), `dispatch_comb_with_args`
(`src/runtime/methods_dispatch_match.rs`) and `extract_extrema_adverbs`
(`src/runtime/builtins_collection_extrema.rs`). The positional `ValuePair`
flavour keeps its slot in all four, so `min((a => 1), (b => 2))` still compares
`Pair`s.

## Result

The 57-probe survey against `raku` went from 38 divergences to 9, of which three
(`pick`, `roll`, `Bag`) are only element ordering. `make test` is green
(3428 files / 32397 tests), as is a targeted 1014-file roast sweep across
`S02-`…`S32-`.

The six genuine leftovers all belong to symptom 2 in code that has *no* named
parsing to extend — `.chop(:x)`, `.polymod(3, :x)`, `.fmt("%d", :x)` — or where
a named-flavour `Pair` can legitimately arrive as data (`.classify`, `.rotor`),
plus `.first(:x)`, which Rakudo rejects with `X::Adverb` rather than swallowing.
Closing those soundly needs the per-method accepted-named declaration the ticket
predicted; it is recorded, with the measured evidence, in
`todo/deep/native-method-accepted-named-declarations.md`.
