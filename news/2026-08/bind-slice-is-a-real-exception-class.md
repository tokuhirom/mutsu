# `X::Bind::Slice` is a real exception class

Binding to a Whatever subscript (`@a[*-1] := 42`) is illegal, and mutsu's
compiler already emitted the right throw for it — `die X::Bind::Slice.new`. The
class was never registered, so `.new` did not exist on it and the throw itself
failed with a different exception:

```
$ mutsu -e 'try { my @a; @a[*-1] := 42 }; say $!.^name'
X::Method::NotFound
```

Registering it makes the raise land, and the raise site now passes the same
attributes rakudo's carries, so the whole exception matches:

```
$ mutsu -e 'try { my @a; @a[*-1] := 42 }; say $!.^name; say $!.message; say $!.type.^name'
X::Bind::Slice
Cannot bind to Array slice
Array
```

byte-identical to `raku`'s answer for the same program.

**It is registered under `Exception`, not `X::Bind`.** Despite the name,
`X::Bind::Slice ~~ X::Bind` is False in rakudo — `.^parents(:local)` is plain
`Exception`. The namespace-prefix rule that would have suggested `X::Bind` is
wrong more often than right across the `X::` hierarchy, because rakudo carries
the shared behaviour in roles rather than superclasses; that is written up, with
the measurements, in
`todo/deep/exception-class-hierarchy-is-mostly-unregistered.md` — 124 core `X::`
classes are still unregistered and the sweep needed to fix them is a design
question, not a mechanical one.

Found by the full Test-vendoring sweep
(`todo/tickets/compile-errors-that-name-no-exception-class.md`): mutsu's native
`Test` provider was lenient enough to accept the wrong class, rakudo's real
`Test.rakumod` is not. Two of the nine files in that ticket
(`t/bind-to-whatever-index.t`, `t/indexed-bind-in-expression.t`) are cleared.
The first of those grew four assertions covering `.message`, `.type`, the
non-inheritance from `X::Bind`, and direct construction; it is green under
`raku` too.
