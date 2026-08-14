# A sigilless `\name := $var` bind is a real alias, not a readonly snapshot

Triaging `Math::Interval` from the un-triaged `test_die` list in
`todo/tickets/dist-test-suite-failures-batch.md` turned up a general bug in how
mutsu handles sigilless (`\name`) variable declarations.

`lib/Math/Interval.rakumod`'s `TWEAK` for the 2D interval type does:

```raku
my (\x1, \x2, \y1, \y2) := my ($x1, $x2, $y1, $y2);
...
submethod TWEAK {
    ($x1, $x2) = ($!x.min, $!x.max);
    ...
}
```

reducing to the much smaller:

```raku
my $x1 = 5;
my \x1 := $x1;
x1 = 10;
say $x1;   # raku: 10 — mutsu: "Cannot modify an immutable value (x1)"
```

## Root cause

raku's rule for a sigilless `my \name := expr` (or the plain-`=` form, which
means the same thing for a sigilless term since there is no container to
assign into) is: when `expr` is itself a plain variable, `name` becomes a
writable ALIAS of that variable's container — assigning through `name` writes
through to the original variable. When `expr` is any other rvalue (a literal,
a computed expression), `name` stays genuinely readonly.

mutsu's parser (`parse_sigilless_decl` in
`src/parser/stmt/decl/my_decl_helpers.rs`) decided mutability purely from
whether a *type constraint* was written — `my \a := $a` was unconditionally
marked readonly, `my Mu \a := $a` was unconditionally marked writable —
without ever looking at what `expr` actually was. This was wrong in both
directions:

- **Untyped bind to a variable** (`my \a := $a`) was wrongly readonly, even
  though the RHS was a plain container reference.
- **Typed bind to a variable** (`my Int \a := $x`) was marked writable, but
  never actually aliased the source container — it silently became an
  independent local copy, so `a = 10` mutated only `a`, leaving `$x`
  unchanged. That's arguably worse: no error, but silently wrong semantics.
- **Typed bind to a literal** (`my Int \a := 5`) was also wrongly writable
  (`a = 10; say a` printed `10` instead of dying).

The sigilled case (`my $b := expr`) already had the right rule in
`my_decl_assign::handle_binding`: `scalar_binding_rhs_is_readonly` plus a
`bind_to_var` check on the RHS shape, routing a variable-RHS bind through the
existing `MarkBind` → `WrapVarRef` → `bind_source` alias machinery. The fix
extracts that same RHS-shape decision into a shared helper
(`build_sigilless_bind_stmt`) used by all three sigilless forms (`:=`, `::=`,
plain `=`), so mutability now follows what the sigilless name is bound to,
not whether a type was written on it.

## What's still open

`Math::Interval`'s actual `TWEAK` binds four names from a **list
destructuring** in one shot (`my (\x1, \x2, ...) := my ($x1, $x2, ...)`).
That shape still fails: mutsu's destructuring-bind desugaring reads each
element back out of a temp array by index, which loses per-element container
identity entirely — a distinct, deeper gap already tracked in
`todo/deep/element-itemization-lost-in-scalar-binding.md`. So the general
single-variable sigilless-bind bug is fixed and pinned
(`t/sigilless-bind-writable-alias.t`), but `Math::Interval`'s own test suite
still needs that separate architectural fix to pass fully.
