# A statement-modifier `if`/`unless` on `constant`/`my constant` loses the value

```raku
my constant $w = 11 if True;
say $w;
# raku:  11
# mutsu: (Any)
```

Confirmed pre-existing (reproduces identically before and after the
`constant_decl` semicolon fix in
`todo/tickets/cbor-simple-typed-array-and-diagnostic-format-gaps.md`'s
"Undeclared routine: elsif used" investigation — that fix only addressed a
following NEW statement being misparsed as a modifier; this is about a
GENUINE modifier not taking effect).

## Root cause (not yet investigated)

`parse_statement_modifier` (`src/parser/stmt/modifier.rs`) correctly parses
`my constant $w = 11 if True;` as a statement-modifier-wrapped `VarDecl` (it
must, since the file is accepted without a parse error), but whatever the
compiler does with a modifier-wrapped `VarDecl` carrying `__constant` custom
traits does not actually bind `$w` — it ends up `Any` (undefined) rather than
`11`. Plausibly the modifier-wrapping produces an `Stmt::If`-like conditional
execution shape around the `VarDecl`, and the compiler's constant-binding
path (which likely evaluates/registers the constant at a fixed point,
possibly compile-time-ish via `BEGIN`-like handling — see
`constant-begin-initializer.t`) does not know how to handle being nested
inside that conditional, silently skipping the bind.

## Repro

```raku
my constant $w = 11 if True;
say $w;   # raku: 11, mutsu: (Any)
```

Also worth checking whether a bare (non-`my`) `constant $w = 11 if True;`
and a `False`-condition case (`... if False;` — should `$w` even be declared
at all in that case? worth checking against raku) have the same or a
different shape.
