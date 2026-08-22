# `trusts` trait is not honored, and `.^trusts` reflection method is unimplemented

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`,
`Type/Metamodel/Trusting.rakudoc:18` and `:54`).

## Root cause

Two related gaps in `Metamodel::Trusting` support:

1. **The `trusts TYPE;` trait declaration inside a class body has no effect on private
   method dispatch.** A class that declares `trusts A;` should allow `A` to call its
   private methods (via `$obj!ClassName::method()`) even from outside the trusting
   class's own methods. mutsu still throws "does not trust" as if the trait were never
   registered.
2. **`SomeClass.^trusts` (the `Metamodel::Trusting` reflection method that lists the
   trusted types) is entirely unimplemented** — calling it throws `X::Method::NotFound`
   ("No such method 'trusts' for invocant of type 'Perl6::Metamodel::ClassHOW'").

## Minimal repros

```raku
class A {
    my class B {
        trusts A;
        method !private_method() {
            say "Private method in B";
        }
    }
    method build-and-poke {
        B.new()!B::private_method();
    }
};
A.build-and-poke;
```

- `raku`: `Private method in B`
- `mutsu` (`target/debug/mutsu`): `Cannot call private method 'private_method' on
  package B because it does not trust A`

```raku
class A { trusts Int; };
say .^name for A.^trusts;
```

- `raku`: `Int`
- `mutsu`: `No such method 'trusts' for invocant of type 'Perl6::Metamodel::ClassHOW'`

## Affected files (starting point)

- Wherever the `trusts` trait is parsed/registered on a class declaration (grep for
  `"trusts"` in `compiler/`/`runtime/class.rs`) — check whether the trait is recorded
  at all, or recorded but never consulted by the private-method-call permission check.
- The private-method-call guard that currently always rejects cross-class private
  calls (grep for "does not trust" / "Cannot call private method").
- `Perl6::Metamodel::ClassHOW`'s reflection-method dispatch (`.^trusts`) — needs a new
  0-arg reflection method returning the registered trusted-type list, alongside the
  existing `.^methods`/`.^attributes`/etc.
