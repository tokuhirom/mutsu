# `.Capture` on an instance reads the raw attribute value, bypassing an overriding accessor method

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Mu.rakudoc:119`).

## Repro

```raku
class Foo {
    has $.foo = 42;
    has $.bar = 70;
    method bar { 'something else' }
}.new.Capture.say; # OUTPUT: «\(:bar("something else"), :foo(42))␤»
```

- raku: `\(:bar("something else"), :foo(42))`
- mutsu (`target/debug/mutsu`): `\(:bar(70), :foo(42))`

## Analysis

`Foo` declares an attribute `$.bar` (which would normally auto-generate a `bar` accessor
returning the stored value `70`) but also explicitly declares `method bar { 'something else' }`,
which overrides the auto-generated accessor. Calling `.bar` on an instance correctly dispatches to
the explicit method (not the attribute default) per normal method-resolution rules. `.Capture`
should build its pairs by calling each public accessor *method* (so it picks up the override and
reports `"something else"`), but mutsu's `.Capture` conversion appears to read the raw stored
attribute value directly, bypassing the method-dispatch/override step entirely.

## Affected files (starting point)

- The built-in `.Capture` coercion for class instances — find where it enumerates public
  attributes to build the resulting `Capture`'s named pairs; it needs to invoke the (possibly
  user-overridden) accessor method for each, not read the attribute store directly.
