# Two exception shapes the real `Test` module checks

Both found by the Test-vendoring sweep
(`todo/tickets/vendor-real-test-module.md`), and both independent of it.

## A type declared in EVAL'd code is not an undeclared routine

Raku resolves routine names at compile time, so mutsu scans EVAL'd code for
calls to names nothing declares and raises `X::Undeclared::Symbols` before the
unit runs. That scan (`check_eval_undeclared_routines`) collected `sub`,
`method` and `enum` declarations — but not `class`, `role` or `subset`. A **type
name is legitimate in call position**: `99 but R("x")` initializes a role's
single public attribute, and `Foo(1)` is a coercion. So

```
$ mutsu -e 'use MONKEY-SEE-NO-EVAL; EVAL q{my role R { has $.x }; 99 but R("ok")}'
Undeclared routine:
    R used at line 1
```

died before running perfectly good code. The scan now collects class / augment /
role / subset names, walking their bodies for nested declarations, and the
genuinely-undeclared case still raises as before.

## `X::Phaser::PrePost` carries its message

A failing `PRE`/`POST` phaser raised the right class with the right `.phaser` and
`.condition`, but built the exception instance without a `message` attribute —
and `.message` reads that attribute, so it came back empty:

```
$ mutsu -e 'use MONKEY-SEE-NO-EVAL; try EVAL q{my sub a { PRE 0 }; a()}; say "[" ~ $!.message ~ "]"'
[]                                # raku: [Precondition '0' failed]
```

Every `throws-like ..., message => /.../` assertion against it therefore failed.
The four raise sites (two of which built the exception by hand, one of them
labelling a failing `POST` as a *Pre*condition and using the phaser name where
the condition belonged) are now one `runtime::phaser_prepost_error` helper that
derives the message the way raku does and puts it on both the instance and the
`RuntimeError`. `t/phaser-prepost.t` passes under the real module as a result.

## What this did *not* fix

`t/role-initialization.t` is still red under the real module, but for a
different reason that the first fix uncovered: a `my role R` declared inside an
EVAL that runs under a non-`GLOBAL` package registers as `Mod::R`, and the bare
`R(...)` in call position then does not resolve ("Unknown function: R") — raku
resolves it lexically. That is recorded with the related
`context`-argument problem in
`todo/deep/eval-context-argument-is-ignored.md`.

Pinned by `t/eval-type-decl-and-phaser-message.t`, whose 9 assertions are green
under `raku` too.
