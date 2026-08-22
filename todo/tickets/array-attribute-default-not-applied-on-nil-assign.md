# `has @.attr is default(V) is rw` — assigning `Nil` doesn't reset the array to `[V]`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Attribute.rakudoc:58`).

## Root cause hypothesis

For a **scalar** `rw` attribute with `is default(V)`, assigning `Nil` correctly resets it to `V`
(this already works in mutsu — see the `C` class in the repro below). For an **array-sigiled**
`rw` attribute with `is default(V)`, assigning `Nil` should likewise reset the array to a
single-element array containing the default (`[V]`) — but mutsu leaves the array holding `[Any]`
(i.e. it does apply *some* per-element default behavior, since the array isn't left as the plain
assigned `Nil`, but the per-element default value it picks is the type-object `Any` rather than the
attribute's declared `is default(42)` value). This suggests the `@`-sigil assignment/reset path
consults a *generic* per-element default (whatever an untyped `Array` slot defaults to) instead of
threading through the attribute's own `is default(...)` trait value the way the scalar path does.

## Minimal repro

```raku
class Foo {
    has @.bar is default(42) is rw
};
my $foo = Foo.new( bar => <a b c> );
$foo.bar = Nil;
say $foo;
```
- `raku`: `Foo.new(bar => [42])`
- `mutsu`: `Foo.new(bar => [Any])`

Compare with the scalar case, which mutsu already gets right:
```raku
class C {
    has $.a is default(42) is rw = 666
}
my $c = C.new;
$c.a = Nil;
say $c;
```
- `raku` and `mutsu` agree: `C.new(a => 42)`

## Affected files (starting point)

- Wherever array-attribute assignment/reset applies the `is default(...)` trait — likely near the
  scalar-attribute `is default` handling that already works, but on the `@`-sigil container path.
  Search for `is default` / attribute default-value application in `src/runtime/class.rs` and the
  compiler's attribute-assignment compilation.
