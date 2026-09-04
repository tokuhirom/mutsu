# A sigilless alias to a not-yet-existing element vivifies on write

```
raku  -e 'my @a; my \p := @a[5]; p = 9; say @a.raku'   # [Any, Any, Any, Any, Any, 9]
mutsu -e '...same...'   # before: Cannot modify an immutable Package ((Any))

raku  -e 'my %h; my \p := %h<x>; p = 9; say %h.raku'   # {:x(9)}
mutsu -e '...same...'   # before: Cannot modify an immutable Package ((Any))
```

## What the ticket got wrong, and what localised it

`todo/tickets/alias-to-a-missing-element-does-not-vivify.md` claimed "the
`$`-sigil twin diverges the same way" and pointed at the token machinery. It
does not: `my @a; my $p := @a[5]; $p = 9` has always given
`[Any, Any, Any, Any, Any, 9]`. The inference came from a LIST spelling
(`my $l := (@a[5],); $l[0] = 9`), which failed for the separate reason that a
list literal did not carry element containers at all — fixed by
`news/2026-09/list-literal-element-container.md`.

Both spellings compile to the same ops up to the store:
`IndexAutovivifyLazyTerminal` puts a deferred vivification token on the stack
and `WrapVarRef` tags it. The only difference is that the sigilless one then
runs `MarkSigillessBindSource`, which settles the term's mutability from what
the bind source denotes — and a token is not a `ContainerRef`, an
`Array`/`Hash`, or a `Proxy`, so it settled the term readonly.

## Fix

A deferred token (`ValueView::HashEntryRef`) joins that set. It denotes an
element that does not exist yet; the binding still names it, and the store
already knows how to resolve the token when a write arrives — which is exactly
what the `$`-sigil spelling relies on.

Reading the alias first still does not vivify (`my @a; my \p := @a[5]; p` reads
`Any` and leaves `@a` empty), an existing element still writes through, and a
bind to a plain value is still immutable.

## Coverage

`t/sigilless-bind-missing-element.t` — 12 assertions, all dual-oracled against
rakudo: array, hash, nested-path and through-a-list-literal vivification; the
read-does-not-vivify rule for both sigils; the `$`-sigil controls; and the
existing-element and value-bind controls. `t/bind-alias-is-a-container.t` (34),
`t/sigilless-bind-writability-source.t` (16), `t/sigilless-bind-chain.t` (14)
and `t/list-literal-element-container.t` (15) stay green, as do `make test`
(3649 files) and a 282-file targeted roast sweep.
