# `.of` on a class statically `does`-ing a parametric `Associative[K,V]` reports the wrong value type

Discovered via the doc-diff harness on `raku-doc/doc/Type/Associative.rakudoc` (around line 53).

## Minimal repro

```raku
class DateHash is Hash does Associative[Cool,DateTime] {};
my %date-hash := DateHash.new;
say %date-hash.of;
```

- `raku`: `(Cool)`
- `mutsu` (`target/debug/mutsu`): `(Mu)`

The class composes without error (no crash), and `%date-hash.of` returns *a* type object, just
the wrong one — `Mu` (the generic fallback / "no constraint") instead of the declared value-type
parameter `Cool` from `Associative[Cool, DateTime]`.

## Relationship to other tickets

This is a *different* code path from the already-filed
`builtin-parametric-role-mixin-not-composable.md`, which is about the runtime `but` mixin
operator (`%hash but Associative[Int, Int]`) failing to compose at all because `but`'s built-in
role lookup doesn't recognize `Associative` as a role name. Here, composition via a **static
class declaration** (`class Foo is Hash does Associative[K,V] {}`) already works — the bug is
that the composed parametric role's type parameters aren't threaded through to the `.of`
reflection method.

## Root cause hypothesis

The `.of` method (typed-container introspection, already correctly used elsewhere for e.g.
`Array[Int].of`) presumably reads a type-parameter slot that gets populated when a class is
declared with the built-in `Array[T]`/`Hash[K,V]` *type-parameterization* syntax, but is not
populated when the value type instead comes from a `does Associative[K,V]` role composition in
the class body. These are two different mechanisms in Raku for expressing the same
"parameterized container" concept, and mutsu's `.of` implementation likely only consults the
former.

## Affected files (starting point)

- Wherever `.of` is implemented (native method, `src/builtins/methods_0arg/` or
  `src/runtime/methods.rs`) — needs to also check a composed `Associative[K,V]`/`Positional[T]`
  role's type arguments (recorded somewhere during class-body `does` role composition), not just
  the built-in parametric-package type-argument slot.
- `src/runtime/registration_class_body_*.rs` — class-body role composition, to see whether/where
  a composed parametric role's type arguments are stored on the class metadata at all.
