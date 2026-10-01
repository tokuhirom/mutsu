# Warnings raised inside an `END` body are swallowed, and `&!attr` is checked

Two loose ends of the #10507 ticket, one of which turned out to be a different
bug from the one it described.

## `END` bodies swallow unhandled warnings

The ticket recorded that a never-reached `END` reading a seeded lexical warned
when it interpolated it:

```raku
my $t = 5; if False { END { say "t=$t" } }
# rakudo: t=          mutsu: "Use of uninitialized value ..." then t=
```

The seed (`Any`, see `news/2026-09/unreached-end-phasers-see-unassigned-lexicals.md`)
is right; the warning is not about the seed at all. Measured against rakudo
2026.07, *no* warning ever escapes an `END` body: not an explicit `warn`, not the
uninitialized-value warning of a lexical the body legitimately declared, not
the numeric-context one, and not one under `try`:

```raku
END { warn "w1"; say "after" }          # after
my $u; END { say "u=$u" }                # u=
my $u; END { say +$u }                   # 0
```

rakudo runs `END` after the mainline's warning handler is gone, so a `warn` that
nothing in the body handles is dropped. A `CONTROL` handler the body installs
itself still sees it (`END { CONTROL { default { say .message } }; warn "w" }`
prints the message), and a warning raised by the mainline is reported as usual.

mutsu already has the right primitive: the warning suppression `quietly` uses
(`push_warn_suppression`), which mutes unhandled warnings but lets a handler
registered *inside* the region run. `Interpreter::finish` now holds it open
around the whole END run, restoring it on the early-return path of a dying
phaser. No special case for seeded lexicals is needed, and none was added: the
earlier, narrower reading ("an uncloned closure's container stringifies
quietly") would have left `END { warn "w" }` and a reached END's own
uninitialized value noisy.

`t/control/end-phaser-unhandled-warnings-swallowed.t` pins the nine shapes.

## `&!attr` is checked like `$!attr`

The ticket's first symptom (`EVAL` reporting `@!nope` / `%!nope` as
`X::Undeclared` instead of `X::Attribute::Undeclared`) no longer reproduces: the
typed-visitor port of the EVAL checks (`eval_var_scan.rs`) exempts every
`!`-twigil name, leaving it to the attribute check. What was still wrong is the
neighbour `&!nope`, which the attribute check ignored entirely (no error, as a
program or under `EVAL`; rakudo says `Attribute &!nope not declared in class C`).
`AttrScan` now treats `NameKind::CodeVar` like the other sigils.

`t/oo/attribute/undeclared-attribute-in-every-position.t` gains the `EVAL`
forms of all three, the program form of `&!`, and a declared `&!` attribute that
must still pass. Its stale comment ("EVAL rejects an undeclared `@!x` earlier,
as X::Undeclared") is gone.

## Oracle note

The rakudo in the remote container is 2026.09, which raises a raw `VMNull`
failure (`boot-code dispatcher only works with MVMCode`) on *any* use of a
never-reached `END`'s seeded lexical; the expectations above, and in
`end-phaser-compile-time-install.t`, were measured on 2026.07
(`install-raku.sh --version 2026.07 --prefix tmp/...`).
