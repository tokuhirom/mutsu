# `.self` does not decontainerize, so `$a.self =:= $a` answers True

Measured 2026-09-05 against raku v2026.07 and `main` @ `37dd63f33`, while
surveying the raw-invocant method family for
[ADR-0067](../../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md).

## Repro

```raku
my $a = 42;        say ($a.self =:= $a);   # raku: False   mutsu: True
class C {}; my $c = C.new;
                   say ($c.self =:= $c);   # raku: False   mutsu: True
my @a = 1, 2;      say (@a.self =:= @a);   # raku: True    mutsu: True    (agrees)
my $a = 42;        say ($a.item =:= $a);   # raku: True    mutsu: True    (agrees)
```

## Why it matters

`.self` looks like it belongs to the raw-invocant family (`.item`, `.snitch`)
and it does not. In Rakudo `.item` hands the invocant's **container** back
(`$a.item =:= $a` is `True`, and `$a.item = 5` writes `$a`), whereas `.self`
hands back the **value** — `$a.self = 5` is refused, and the identity test is
`False` for anything held in a scalar container.

mutsu returns the invocant unchanged for `.self`, container and all, so it
reports `True` where raku reports `False`. The `@`/`%` rows agree because those
sigils pass the container itself either way.

ADR-0067 lists this as an explicit non-goal so the family survey does not sweep
`.self` in by mistake; it is recorded here so the divergence itself is not lost.

## Scope

Small and self-contained: make `.self` decontainerize its invocant. The risk to
check before shipping is the `@`/`%` rows above, which must keep answering
`True`, and any internal caller that uses `.self` as an identity-preserving
no-op.

## Not a duplicate of

- `todo/deep/native-method-cannot-return-an-lvalue-container.md` — that is the
  opposite direction (a method that *should* hand back a container and does
  not). This one hands back a container it should not.
