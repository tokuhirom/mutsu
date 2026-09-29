# A `:=` rebind of a file-scope lexical is now seen by named subs and class methods

`my $l; sub f { $l }; $l := 5; f()` returned `Any`, and a class method that
read the lexical saw its old value. The reverse held too: a method that did
`$list := values.List` never reached the mainline or its sibling methods.

The declaring frame boxes a lexical a named sub captures, but it gave the box a
*binding cell* (a cell whose content is the variable's container, ADR-0097)
only when the sub itself rebound the name. It now also does so when the
declaring frame rebinds the slot, both when `RegisterSub` captures it and when
`box_decl_local_cell` boxes it at the declaration. A method body's own rebinds
of an outer lexical now bubble up to the declaring frame the same way a nested
closure's do.

Two neighbours fixed on the way, both found by List::Agnostic's
`t/01-basic.rakutest` (now 29/29):

- `my @m is Tied = ...` calls the user `STORE`; an outer lexical rebound inside
  that `STORE` is now pulled back into the caller's slot after the call.
- `my @o := $obj` accepted an instance only when its class composed `Positional`
  directly. Positional reached through a composed role's own roles
  (`role R does Positional`, `role S does R`, `class C does S`) is now accepted.

Pin: `t/vm/binding/rebind-seen-by-named-subs-and-methods.t`.
