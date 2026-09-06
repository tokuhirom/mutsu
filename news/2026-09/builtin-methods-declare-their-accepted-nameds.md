# Builtin methods declare the named arguments they accept

`"abc".chop(:zzz)` answered `"abc"`. `10.polymod(3, :zzz)` answered
`(1, 3, Inf)`. `(1,2,3).classify({$_}, :zzz)` produced four buckets. In every
case the call *succeeded*, with the wrong answer and no error to notice.

This was the silent half of "a named argument may occupy a positional slot".
The loud half — where an unknown named made the arity-keyed builtin lookup miss
entirely, and `4.log(:base(2))` died with `X::Method::NotFound` — was fixed in
August by an implicit-`*%_` retry in `call_method_with_values`
([news](../2026-08/native-methods-honour-the-implicit-slurpy-named.md)). The
retry cannot help when the *wrong arm hits*: mutsu's builtin methods are
dispatched by arity rather than by signature, so a call-site `Pair` is counted
as a positional and then numified (a 0 character count for `chop`), read as
data (a second modulus for `polymod`, a second cycle spec for `rotor`, an extra
list element for `classify`) or rejected as surplus (`sprintf`).

## The measurement came first, and it moved

`todo/deep/native-method-accepted-named-declarations.md` recorded six affected
calls. Two of them had already drifted — `3.fmt("%d", :zzz)` no longer died with
an `X::AdHoc` about a surplus sprintf argument but with a positional-arity error
— so the first step was a fresh, wider sweep: 2 600 (receiver, method,
argument-shape) probes generated from `src/builtins/native_method_row_table.rs`,
each run as `R.M(A)` against `R.M(A, :qqzz9)`, with every divergence then checked
against `raku`. **754 probes were not named-blind**, across about 30 methods —
five times the recorded population. `fmt` on three owner types, `sprintf`,
`base`, `expmod`, `minmax`, `AT-POS`, `EXISTS-POS`, `subbuf`, `categorize`,
`join`, `tail`, `skip`, `combinations`, `indent`, `samecase`, `uniprops` and
`int-bounds` were all affected and none had been recorded.

The sweep also corrected the finding's framing. It said `chop`/`polymod`/`fmt`
"have no named parsing at all to extend", implying they differ structurally from
`rotor`/`classify`. They do not — only in *which* names they accept (none,
versus `:partial` and `:as`/`:into`). And the `classify` caveat about a
named-flavour `Pair` legitimately arriving as data turned out not to apply to
the argument list at all: `callable_item`'s comment is about `Pair`s that arrive
as **elements of the list being classified** (a `Hash`/`Bag` invocant iterating
to pairs), which never pass through the argument vector.

## The mechanism

[ADR-0070](../../docs/adr/0070-native-methods-declare-the-named-arguments-they-accept.md).
`native_method_accepted_nameds(method) -> Option<&'static [&'static str]>` in
`src/builtins/accepted_nameds.rs` states which names each surveyed method reads,
and the builtin dispatch layer drops everything outside that set before it
chooses an arity. It is consulted at exactly the three places that layer is
entered — `try_native_method_raw`'s arity cascade, its interpreter-side twin in
`methods_call_dispatch.rs`, and `dispatch_method_by_name_1/2/3` — and the
filtered list is a local, so a user method, a constructor and every
`%_`-carrying handler downstream still see the call's own arguments.

The table is **partial on purpose**, and the polarity is the whole decision. An
unsurveyed method is `None` and behaves exactly as before, so an omission leaves
a known-wrong answer wrong rather than inventing a new one; a *wrong* row (a
method declared that really does read an adverb) makes that adverb stop working,
which is loud. The other polarity — "the cascade is named-blind unless the
method is listed" — has the failure modes the other way round, and would delete
real adverbs on an omission.

The ADR also records why the file's second candidate design (extend the
`native_*_with_options` interceptor pattern until no cascade arm reads an
argument `Pair`) was rejected on the measurement: the `arity_bits` column shows
only about half the affected methods are served by the pure cascade at all —
`polymod`, `rotor`, `classify`, `categorize`, `first`, `minmax`, `skip` and
`sprintf` are slow-path — so it would close half the finding, and the cascade is
already so close to named-blind (five arg-side named reads in the whole tree)
that its "structural invariant" reduces to a five-entry exemption list guarding
the same filter.

`grep` and `first` are deliberately not in the table: Rakudo does not let their
implicit slurpy swallow an unknown adverb, it **validates** and answers
`X::Adverb` (`grep` throws, `first` `fail`s). mutsu already did that for `grep`;
`first` now does too, in both the method and the sub form, so
`(1,2,3).first(:zzz)` is a Failure carrying `X::Adverb` with `.what`,
`.source` and `.unexpected` rather than a silent `Nil`.

## The guard

The sets are not hand-guessed. `scripts/native-method-adverb-survey.raku` asks
Rakudo for the named parameters every candidate of a method declares across
every owner type — authoritative, because Rakudo's implicit `*%_` means a name
outside that set *cannot* change the answer. It flags the `*%_` slurpy
explicitly, so a routine that validates its own `%_` is visibly not answerable
by signature alone and is left out.

`t/native-method-accepted-nameds.t` pins both halves of every declaration — an
undeclared named is invisible, and every declared adverb still reaches the
implementation (`rotor(:partial)`, `classify(:as)`/`(:into)`, `minmax(:by)`,
`first(:k)`/`(:kv)`/`(:end)`), plus the positional `Pair` cycle spec
`rotor(2 => -1)` that ADR-0021's flavour distinction protects. **The file passes
unmodified under `raku` as well as under mutsu**, so it is a conformance test,
not a snapshot of mutsu's own behaviour.

## Result

332 of the 754 non-named-blind probes became named-blind, including every row of
the original finding. No probe that was named-blind before stopped being so; the
only new divergences in the re-run sweep are `first`'s intended `X::Adverb` and
pre-existing QuantHash iteration-order noise. The remainder is undeclared
methods plus receiver shapes whose *plain* call already diverges for unrelated
reasons (`4.roots(2)` answers `Num`s where raku answers `Complex`), and stays
recorded in the narrowed `todo/deep/` file.
