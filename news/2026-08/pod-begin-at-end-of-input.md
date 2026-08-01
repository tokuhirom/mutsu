# `=begin` at end of input, and `lives-ok` takes a `Callable`

Two more findings from the Test-vendoring sweep
(`todo/tickets/vendor-real-test-module.md`), one an interpreter fix and two test
files corrected.

## `=begin` with no identifier, at end of input

`=begin` with no block name is `X::Syntax::Pod::BeginWithoutIdentifier`. mutsu
raised it when the `=begin` was followed by more source, but not when it was the
very last thing the parser saw — there the directive was not recognised as Pod
at all and the unit failed with a generic parse error.

That case is not exotic, because **`EVAL` trims its argument**: `EVAL "=begin\n"`
reaches the parser as a bare `=begin`, which is exactly the shape
`throws-like "=begin\n", X::Syntax::Pod::BeginWithoutIdentifier` produces. The
end-of-input arm now raises the same typed error the rest-of-line check does,
and both share one builder.

raku distinguishes the two spellings — with no trailing newline it does not read
`=begin` as a Pod directive at all and reports an infix `=` in term position —
but mutsu cannot, because the trim has already erased the difference by the time
the parser runs. Matching the newline form is the useful half: it is the one real
source ever contains, and it is what every assertion against this error is
written as.

## Two test files that asserted more than Raku does

- **`t/pod-begin-without-identifier.t`** asserted that `"say 1; =begin\nsay 2;"`
  is `X::Syntax::Pod::BeginWithoutIdentifier`. A Pod directive has to *start a
  line*, so raku reads that `=begin` as an infix `=` in term position instead.
  The `=begin` gets its own line now, which is what the assertion meant.
- **`t/variable-traits.t`** called `lives-ok` with a `Str`. `lives-ok` takes a
  `Callable`; the string form is `eval-lives-ok`, and raku rejects the call at
  compile time (*Calling lives-ok(Str, Str) will never work*). mutsu's native
  provider accepted it. It was the only such call in the whole of `t/`.

Both files are green under `raku` now, as is `t/pod-begin-at-end-of-input.t`,
which pins the parser fix.

## What still keeps `t/variable-traits.t` red under the real module

`Test.rakumod` exports `multi sub trait_mod:<is>(Routine:D $r,
:$test-assertion!)`. Once that is imported, mutsu routes *every* `is` trait
through user multi-dispatch, so an unknown trait comes back as `X::Multi::NoMatch`
("Cannot resolve caller trait_mod:<is>(Any:D)") instead of falling back to the
built-in handling and its `X::Comp::Trait::Unknown`. Recorded in
`todo/tickets/user-trait-mod-multi-shadows-builtin-traits.md`.
