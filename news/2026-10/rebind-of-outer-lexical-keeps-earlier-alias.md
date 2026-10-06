# Rebinding an outer lexical no longer moves a name bound to its old container

A routine that binds a local name to an outer lexical and then rebinds that
lexical now leaves the local name on the old binding, as Rakudo does:

```raku
my $x := [1, 2];
sub t() { my $n := $x; $x := []; $n.elems }   # 2
```

mutsu answered `0`, because the local followed the rebind. The same happened for
a module file's own `my` (with or without `unit`), which is the shape of
Rakudo's `Telemetry::periods` (`my $new := $snaps; $snaps := nqp::create(...)`),
so the default-snapshot form of the vendored `Telemetry` reported nothing
([#11797](https://github.com/tokuhirom/mutsu/issues/11797)).

Four pieces were wrong, each in a different store:

- **The by-name alias propagation.** `SetGlobal` re-resolved every name that was
  once `:=`-bound to the rebound variable and wrote the new value into it. A
  rebind installs a new binding, so it now propagates to none of them.
- **A bind took the source's binding cell.** `my $n := $outer` bound `$n` to the
  cell that a later rebind of `$outer` re-seats (ADR-0097 §14). It now binds to
  the container behind it, and the source keeps its binding cell wherever it is
  recorded (slot, compunit store or env).
- **A module's own lexical was not an outer variable to the bind.** A module
  routine's source lived in the compunit's lexical store, which no frame env
  vouches for, so the bind fell back to the by-name alias.
- **A rebind wrote through the module store's cell.** With no binding cell to
  re-seat, the by-name write went through the cell that earlier aliases share. A
  module's file-scope lexical (whose declaring frame has finished) now has its
  store binding replaced by a fresh container.

A fifth, unrelated symptom came out of the same repro: after a rebind made by
one routine, a sibling routine that had already run kept pushing into the old
container, because TRIR memoizes a routine's free-variable bindings and a rebind
did not invalidate that memo. A by-name rebind now bumps the generation the memo
is keyed on.

`t/vm/scope/bind-rebind-outer-lexical-leaves-earlier-alias.t` pins the script,
non-unit module and `unit module` shapes. A `module Foo { my ... }` block and a
class body's `my` (the `package_lexicals` store) still follow the rebind
([#12129](https://github.com/tokuhirom/mutsu/issues/12129)), and a script file
that itself starts with `unit module` does not see a rebind from its routines at
all ([#12130](https://github.com/tokuhirom/mutsu/issues/12130)).
