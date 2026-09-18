# WebDriver2: two general interpreter bugs fixed, blocked_load to mostly-green

Working the [`ecosystem-dist-roulette`](../../.agents/skills/ecosystem-dist-roulette/SKILL.md) draw
(locked on [#7884](https://github.com/tokuhirom/mutsu/issues/7884)) landed on **WebDriver2 0.1.12**,
a `blocked_load` record. Its own load probe revealed a real mutsu bug (`WebDriver2::SUT::Tree` and
nine other submodules failed to load with "No matching candidate found for the parametric role"),
and fixing it exposed a second, unrelated bug once the suite could actually run.

## Bug 1: `role R[ Namespaced::Type ::T ]` with a leading space silently dropped its type parameter

`parse_optional_role_type_params` (`src/parser/stmt/class/role_decl.rs`) parses a role's bracketed
type-parameter list by calling `parse_param_list` on the bracket's contents — but without trimming
the whitespace that follows the `[`. Every other call site of `parse_param_list` in the parser trims
first; this one didn't, so `role Resolvable[ Foo::Context ::T ]` (the common style, written with a
leading space) always failed that primary parse attempt and fell through to a naive fallback. That
fallback splits the parameter text on the first literal `"::"` to separate a nominal constraint from
a `::T` capture — correct for a bare constraint (`Context ::T` has exactly one `::`), but wrong for a
*namespaced* one: `Foo::Context ::T` has two, and splitting on the first one yields `"Foo"` (not a
known type) as the constraint and silently drops the capture. The role registered with **zero** type
parameters, so any application supplying an argument (`Resolvable[ Foo::Context ]`) failed arity
matching with "No matching candidate found for the parametric role" — even though the argument
satisfied the constraint.

The fix trims the whitespace before the primary `parse_param_list` call, so the leading-space form
parses correctly the same way the no-space form already did. Regression test:
`t/oo/role/role-qualified-constraint-type-capture.t`.

## Bug 2: a typed array's element-type check didn't dereference a `ContainerRef`

With the load bug fixed, `t/01-basic/build-basic.t` and `t/01-basic/navigator-basic.t` still failed:
"Type check failed for an element of @children; expected WebDriver2::SUT::Tree::ANode but got Any".
Reduced to a standalone repro: a typed Hash or Array holding an instance of a class that satisfies
the element role only *indirectly* (`class Fr does AFrame; role AFrame does ANode`) fails a typed-array
element check on it — but only when the value passes through `.values()` first, not on a direct
element read or a plain list-literal assignment.

Root cause: an array slot or hash entry can hold a `ContainerRef` cell rather than the bare value —
mutsu's element-type checker (`type_matches_value`, `src/runtime/types/type_matching.rs`) already
dereferenced a `Scalar` wrapper before checking an element's type, but not a `ContainerRef`/
`ContainerView`. The opaque cell shape fell through every `Instance`-specific check in the general
type matcher and reported as `Any`. A second, smaller instance of the same asymmetry: `Hash.values()`
already decontainerized its entries; `Array.values()` did not.

Both are fixed — `type_matches_value` now derefs a container cell the same way it already derefs a
`Scalar`, and `Array.values()` matches `Hash.values()`'s existing decontainerization. Regression test:
`t/types/coercion/typed-array-element-container-ref-role-check.t`.

## Bug 3 (smaller, same distribution): `.absolute`/`.relative`'s named `base` argument corrupted the path

`IO::Path.absolute`'s `$base` parameter is positional-only (`method absolute(IO::Path:D: $base --> Str)`);
calling it with a named argument (`.absolute(base => $cdir)`, as `WebDriver2::SUT::Tree::URL.new` does)
doesn't bind it at all in real Rakudo — the zero-arg `$*CWD`-based candidate runs instead. mutsu's
native implementation grabbed `args.first()` regardless of whether that argument was named, so it
picked up the stray `Pair` itself and stringified it (`(base => $cdir).Str` is `"base\t..."` in Raku),
producing a `"base\t/some/path"`-prefixed result instead of ignoring the argument. Fixed by using the
existing `positional_value` helper (already used by every other call site needing "the Nth positional
argument") instead of `args.first()`. Regression test: extended `t/nativecall/native-io-path-cwd.t`.

## Result

WebDriver2 0.1.12 moves from `blocked_load` (0 baseline files, 0 assertions visible) to 4 of 6
baseline files at full parity (`t/00-sanity/use.t` 48/48, `t/01-basic/build-basic.t` 30/30,
`t/01-basic/navigator-basic.t` 14/14, `t/01-basic/tree-basic.t` 20/20). The remaining two files
(`t/01-basic/multi-page.t`, `t/01-basic/visit-tree.t`) hit a third, apparently unrelated grammar
alternation/backtracking bug — filed as
[#8700](https://github.com/tokuhirom/mutsu/issues/8700) rather than chased further in this PR.
