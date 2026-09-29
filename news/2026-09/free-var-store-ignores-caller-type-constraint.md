# A caller's typed `my` no longer constrains a routine's write to its own free variable

A routine that assigned to a free variable of its own lexical outer scope was
type-checked against a same-named typed `my` declared in its *caller*
([#10049](https://github.com/tokuhirom/mutsu/issues/10049)):

```raku
my $x = 'outer';
sub init() { $x = 42 }
{ my Str $x = 'a'; init() }   # died: expected Str but got Int (42)
```

The write itself already landed in the right variable. A routine's store to a
captured mainline lexical (ADR-0024) or to its compunit's file-scope lexical
(ADR-0039) goes through the shared cell `unit_lexical_slot` resolves. The type
check did not follow it: `SetGlobal` read the name-keyed `__mutsu_type::x`
entry out of `env`, and a named-sub call runs on a child of the caller's env,
so the caller's `my Str $x` was the constraint the callee saw. `Test.rakumod`'s
`_init_io` hit exactly this — it writes its file-level `my $output`, so a test
file holding `my Str $output` made `plan`/`ok` die (the `Green` distribution's
`t/02-concise.t`).

Hiding the caller's entries at the call boundary would have been unsound,
because the name-keyed entry was also the only thing enforcing a typed outer
scalar written from a routine (`my Int $x; sub f() { $x = 's' }`). Instead, the
store now takes its constraint from the cell it lands in, following ADR-0042's
"a type constraint belongs to the container". When the free-variable store
resolves to a cell, that cell's `of` is authoritative, and an untyped cell means
no constraint. The same goes for the Nil reset: `$x = Nil` from the routine now
seeds the outer variable's type object, not the caller's. Every other by-name
store keeps the name-keyed lane.

Pinned by `t/routines/free-var-store-ignores-caller-constraint.t`.
