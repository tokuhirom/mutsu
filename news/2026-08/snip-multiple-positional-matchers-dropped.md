# `.snip` honours every positional matcher (and returns a `Seq`)

Discovered via the doc-diff harness on `raku-doc/doc/Type/Any.rakudoc:1525`.
`use v6.e.PREVIEW; .say for (5, 13, 29).snip(* < 10, * < 20)` printed
`(5)` / `(13 29)` where `raku` prints `(5)` / `(13)` / `(29)`.

## Root cause

`dispatch_snip_method` (`src/runtime/methods_dispatch_match2.rs`) did
`let matcher = args[0].clone();` — it passed only the *first* positional to
`eval_snip` and discarded the rest.

`eval_snip` (`src/runtime/builtins_collection_mapgrep.rs`) was already correct:
it takes a list of matchers and advances round-robin, moving to the next matcher
each time a snip is made and dumping everything left into a final group once the
matchers run out. The method dispatcher just never assembled the separate
positionals into that list.

`Any.snip`'s real signature is a bare capture (`($:: |)` — confirmed with
`Any.^lookup("snip").signature`), so every positional is a matcher and
`.snip(* < 10, * < 20)` means the same thing as `.snip((* < 10, * < 20))`.

## Membership check

Per `CLAUDE.md`'s core-routine test, `snip` **is** core: `raku -e '(5,13).snip(* < 10)'`
resolves it with no `use` of any module (it needs `use v6.e.PREVIEW`, a version
pragma, which still counts as core), and it is documented as `=head2 routine
snip` in `raku-doc/doc/Type/Any.rakudoc`. Both conditions hold, so it belongs
where mutsu already had it.

## Fix

`dispatch_snip_method` now wraps two-or-more positionals into a single list
`Value` before calling `eval_snip`, leaving the one-argument case untouched (a
lone `Array`/`Seq`/`Slip` is still read as a list of matchers, matching the
documented `snip (* < 10, * < 20), ...` sub form).

While pinning the behaviour against `raku` it also turned out that `eval_snip`
returned a `List` where `raku` returns a `Seq` of `List`s
(`.snip(...).raku` is `((5,), (13,), (29,)).Seq`), so the result is now a `Seq`.

`t/buf-and-list-mutators.t` covers the doc's two-predicate example, a
three-predicate case, type-object matchers (`.snip(Int, Str)`), the
single-matcher case, the equivalent single-list form, and the `Seq` return type.
