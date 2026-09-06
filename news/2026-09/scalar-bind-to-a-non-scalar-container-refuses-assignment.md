# A `$` bound to a non-Scalar container refuses whole-value assignment

Raku's rule for `$x = v` is sharper than "the thing `$x` is bound to is
immutable": `$x` must be bound to a **Scalar** container, and no other container
qualifies. A real `Array`, a `Hash`, a `Map` and a `Pair` all refuse the
whole-value assignment, though every one of them is mutable through its own
interface.

mutsu accepted all of them. For the most common spelling it did worse than
accept:

```raku
my @a = 1, 2, 3;
my $x := @a;
$x = 5;      # raku: X::AdHoc, "Cannot assign to an immutable value"
say @a;      # raku never gets here; mutsu printed 5 -- @a was gone
```

All seven rows of section C of
`todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` now throw
`X::AdHoc: Cannot assign to an immutable value`, matching raku's class *and* its
wording, and `t/scalar-bind-to-non-scalar-container.t` pins them alongside the
aliasing they must not disturb.

## The fix

`bind_source_is_non_scalar_container`, beside the existing
`bind_source_has_no_container` in `vm/vm_var_assign_set_local.rs`. The two say
different things and are gated differently, which is why they are separate
predicates rather than one widened allowlist: the older one is about
*immutability* and applies to any `:=`, while this one is about *which kind of
container the name owns*.

Three things decided the shape:

- **Two rows never reached the immutability test at all.** `my $x := @a` and
  `my $x := %h` carry a NAMED source, and a named bind is excluded from the
  marking outright because it denotes another variable. They needed their own
  arm, keyed on the source name's sigil, exactly as the survey predicted.
- **The marking has to be restricted to a declaration.** A parameter bind reaches
  the same store, and an `is raw` / `\x` parameter bound to an array must stay
  assignable. Without the `is_vardecl` gate this would have refused writes raku
  performs — the survey's standing warning that "the conservative direction is to
  mark less" applies here too.
- **`my $x := (a => 1)` arrives as `ValueView::ValuePair`, not
  `ValueView::Pair`.** Matching only the latter left that one row still passing,
  which is the kind of thing a per-row test catches and a per-family one does not.

Only the whole-value `=` is refused. The aliasing these binds exist for is
untouched, and the test pins that half explicitly: `$x.push(9)` still grows `@a`,
`$x<b> = 2` still adds the key to `%h`, `$x[0]` still reads through, and a `$`
bound to another `$` is still assignable and still writes through.

## What is left in the survey

Sections A, B, D, E and F. B is the one to read first: those rows are the
*opposite* failure — rakudo performs the write and mutsu silently loses it — so
they must not be "fixed" by teaching the marking to reject more. The survey's own
"how the surviving rows differ" section records two rules that look obviously
right for section A and were measured to break five shapes rakudo accepts; it is
still the thing to read before designing anything there.

One near-miss stays open in this family: `my $x := $(1,2,3); $x = 5` throws
`X::AdHoc` in both, but rakudo words it "Cannot assign to a readonly variable or
a value" where mutsu says "Cannot assign to an immutable value".
