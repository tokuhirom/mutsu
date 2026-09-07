# `my @v is TypeName = ...` now runs Raku's custom-container `STORE` protocol

`Language/subscripts.rakudoc`'s documented custom-container example — the `DNA`
class whose `STORE` validates a nucleotide string and whose `Str` re-groups it in
codons — produced no output at all under mutsu. Neither `STORE` nor the class's
own `Str` was ever reached; `my @string is DNA = 'GAATCC'` left `@string` holding
a plain `Array` with the raw string in it.

## Root cause

The `%`-sigil half of the tie was implemented and correct; the `@`-sigil half was
wrong in three independent ways, all in `src/vm/vm_var_trait_ops.rs` and
`src/vm/vm_var_assign_local.rs`.

**1. The gate was too narrow.** `ApplyVarTrait`'s `@` branch only engaged when the
named type inherited `Array`, composed `Positional`, or defined `AT-POS`. A plain
class that implements only `STORE` — precisely the documented shape — matched
none of those and fell through to the generic `trait_mod:<is>` handler, which
does nothing for a type-named trait. Raku ties an `@` variable to *any* class
named by `is`; the sigil supplies the positional semantics.

**2. It used the wrong protocol even when it did engage.** The branch called
`TypeName.new(|@initializer_values)`, but Raku binds the variable to
`TypeName.new` with **no** arguments and then feeds the initializer through
`STORE(values, :INITIALIZE)`. Measured against rakudo:

```
my @a is T = 1,2,3;   # raku: NEW \()  then  STORE \((1, 2, 3), :INITIALIZE)
                      # mutsu (before): NEW \(1, 2, 3), no STORE at all
```

Besides skipping `STORE`, this crashed outright for any class whose `new` takes
only named arguments (`Default constructor for 'T' only takes named arguments`)
— so the branch was broken even for classes it already claimed.

**3. Reassignment bypassed the tie.** `instance_is_tied` required a composed
`Associative`/`Positional` role *in addition* to a user `STORE`, so a later
`@a = ...` on a tied variable silently clobbered the instance with a plain Array
instead of calling `STORE`. Every caller has already established that the target
is an `@`/`%` variable currently holding an instance, so the extra role
requirement was redundant as well as wrong.

A fourth divergence sat under all of these and affected the already-working `%`
path too: `STORE` was handed its argument wrapped in a list unconditionally.
Raku passes the RHS *as written* — `= 'x'` calls `STORE('x')`, `= 'x','y'` calls
`STORE(('x','y'))` — so a typed single-positional signature like the doc's
`method STORE(Str $chain ...)` failed its type check on a 1-element `List`.
Recovering that distinction needed a new `StashVarDeclInit` opcode: `SetLocal`'s
Array/Hash coercion erases the scalar-vs-list shape (`= 'x'` and `= ('x',)` both
land as a 1-element Array), so the compiler now stashes the raw pre-coercion RHS
for `ApplyVarTrait` to read. The stash is emitted only for `@`/`%` declarations
carrying a type-named `is` trait, is consumed unconditionally at the top of
`ApplyVarTrait` so it can never go stale, and nothing but the custom-container
branches reads it.

Finally, `.VAR` on a tied container reported the base container type (`Array` /
`Hash`) instead of the tie's class. That was pre-existing and sigil-independent —
`%h is Tk` reported `Hash` too — and is fixed here: a tied `@`/`%` variable *is*
its own container, so `.VAR` returns the instance.

## Result

Both examples in `Language/subscripts.rakudoc` §`method STORE` now produce
byte-identical output to rakudo, including the immutable-data-structure variant
built from `multi method STORE(*@!foo, :$INITIALIZE!)` plus a `die "Immutable"`
fallback. `t/custom-container-store-protocol.t` pins twenty rows, every one of
them measured against rakudo first: the DNA example end to end, the argument
shape for scalar/comma-list/array initializers and reassignments, `.^name` /
`.VAR` / `.Str` reflection, `AT-POS`/`ASSIGN-POS` element dispatch, the
`%`-sigil twin, a punned-role tie, and — as a guard against the new branch
swallowing it — the `class A is Array[Str] {}` native-subclass shape that still
legitimately uses the constructor path (`roast/S05-grammar/inheritance.t`
depends on it).

## Corpus scope

This is a documented language feature with thin test coverage upstream. Roast
contains four `method STORE` sites: `S02-types/is-type.t` (an `eval-lives-ok`
that only checks a parameterized `my @a is Foo[...]` parses), `S03-operators/
inplace.t` and `S04-statements/sink.t` (both `.=`/sink coverage, not variable
ties), and none of them exercises `@`-sigil `STORE` dispatch — all five files are
already whitelisted. In `modules/`, DBIish's `DBDish::TypeConverter` uses the
`%`-sigil attribute form (`has %.Converter is DBDish::TypeConverter`), which
already worked. So the change unblocks no new roast file directly; it was taken
because the `@` branch was demonstrably broken *within scope it already claimed*
(bug 2's constructor crash), and because the documented protocol is the kind of
gap that silently produces wrong answers rather than errors.

## Known residue

- `my %h is CustomClass = 'bare-scalar'` still dies with "Odd number of elements
  found where hash initializer expected". The error is raised by `SetLocal`'s
  hash coercion *before* `ApplyVarTrait` runs, so fixing it means perturbing the
  shared hash-coercion path; raku accepts it, but no roast test or bundled module
  uses that shape. The `@`-sigil equivalent — the documented case — works.
- `my @a is PlainClassWithoutSTORE` binds a plain `Array` where raku binds an
  instance of the class (and raku throws `X::Assignment::RO` if such a
  declaration carries an initializer). Also unexercised by the corpus.
- `my @d := SomeNonPositional.new` throws the right exception with a less precise
  message than raku (`expected Positional but got Any` vs
  `... but got T4 (T4.new(items => []))`).
- `is TypeName` on a `$`-sigil variable is not implemented — but neither is it in
  rakudo, which rejects it at compile time with "is trait on $-sigil variable not
  yet implemented".
