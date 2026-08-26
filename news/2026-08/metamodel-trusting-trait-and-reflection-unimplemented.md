# `Metamodel::Trusting`: `.^trusts` reflection, and `trusts` honored for a class nested in a class body

The ticket reported two gaps from `Type/Metamodel/Trusting.rakudoc`: that the `trusts`
trait "has no effect on private method dispatch", and that `.^trusts` is unimplemented.

The second was exactly as described. The first was **not** — `trusts` enforcement is
real in mutsu and always was (`registry().class_trusts`, consulted by
`private_owner_trusts_caller`); a top-level `class B { trusts A }` already let `A` call
`B`'s private methods, and an untrusted caller was already refused. The ticket's repro
happened to use the one shape that failed.

## The real bug behind repro 1: a class nested in another class body

```raku
class A {
    my class B { trusts A; method !private_method() { say "Private method in B" } }
    method build-and-poke { B.new()!B::private_method() }
}
```

A `my class` (or `our class`) declared inside another class body registers under a
mangled lexical storage name — `A::B\u{0}<decl-id>` (ADR-0047 P1) — and that mangled
name is the key `class_trusts` is recorded under. `resolve_private_class_name`
canonicalizes the owner written in `$o!B::meth` purely *lexically*: through the
caller's env, then its enclosing package chain. Inside `A.build-and-poke` the bare name
`B` is no longer bound in the env that check consults, so the owner canonicalized to
the dead bare name `B`, which is never a key in `class_trusts` — and `B`'s own
`trusts A` never matched. Writing the call as `!A::B::private_method()` worked, which
is what isolated it to name resolution rather than to the trait being ignored.

The fix (`resolve_and_check_private_owner_on` in `src/runtime/registration.rs`) adds
the invocant as a second resolution source. `Owner` has to name a type in the
invocant's own MRO for `$o!Owner::meth` to resolve at all, so when the lexically
resolved name is absent from that MRO, the name as written is matched against each MRO
entry's user-facing spelling (mangling stripped), accepting a full-name match or a
trailing `::Owner` segment. The three call sites that have an invocant in hand
(`methods_instance_ops.rs` ×2, `methods_mut_method_lvalue.rs`) pass it; the
non-instance path in `methods_qualified.rs` keeps the lexical-only behaviour.

Enforcement stays real: a nested class *without* `trusts` still refuses an outer
caller, which the test pins explicitly.

## `.^trusts`

New arm in `src/runtime/methods_classhow_dispatch.rs` returning the trusted types as
type objects. Two details came out of measuring `raku` rather than guessing:

* **Order is observable.** `class A { trusts Int; trusts Str }` answers `(Int, Str)`,
  so `class_trusts` changed from `HashMap<String, HashSet<String>>` to
  `HashMap<String, Vec<String>>` (de-duplicated on push, so a re-registered body —
  an `augment`, a re-`EVAL` — does not grow the list).
* **Only `ClassHOW` has the method.** `module M {}; M.^trusts`,
  `enum E <a b>; E.^trusts` and `subset S of Int; S.^trusts` all throw
  `X::Method::NotFound` in Rakudo, while `Int.^trusts` and a grammar's `.^trusts`
  answer `()` (`GrammarHOW` subclasses `ClassHOW`). The arm asks the metaobject itself
  for its metaclass rather than re-deriving the taxonomy, so a new HOW kind cannot
  silently gain the method.

## Tests

`t/metamodel-introspection.t` covers `.^trusts` and the top-level trait; the nested
cases live in their own file, `t/trusts-nested-lexical-class.t`. That split is not
stylistic: a `trusts` declaration inside a nested class body makes every *other* class
in the same compilation unit report a package-qualified `.^name` (`Plain` becomes
`Outer::Plain`), which silently broke four unrelated `:ver`/`:auth`/`:api` assertions
that keyed off the unqualified name. That leak reproduces on unmodified `main` and is
filed as `todo/tickets/nested-trusts-decl-qualifies-sibling-class-names.md`; the two
files can be merged once it is fixed.
