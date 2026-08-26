# The "is this a builtin?" test no longer cites a Perl-migration table

`CLAUDE.md`'s working agreements told agents to decide whether a function belongs in core
by looking it up in `raku-doc/doc/Language/perl-func.rakudoc`:

> **Builtin functions must be listed in `raku-doc/doc/Language/perl-func.rakudoc`.** Only
> implement a function as a builtin if it appears in that file. If a function is not listed
> there, it is NOT a Raku builtin — it comes from a module (e.g. `Test`, `Test::Util`) and
> should be implemented in the appropriate module handler, not as a core builtin.

The rule's *intent* was right and is kept: do not mistake a module's exported function
(`is_run`, `make-temp-dir`, …) for a core routine. But the test it prescribed was wrong,
because that file is not what the rule assumed it was.

## What the file actually is

`perl-func.rakudoc` declares itself a migration document:

```
=begin pod :kind("Language") :subkind("Language") :category("migration")
=TITLE Perl to Raku guide - functions
...
A (hopefully) comprehensive list of Perl builtin functions with their Raku
equivalents with notes on variations between them where necessary.
```

It is an index of **Perl 5's** `perlfunc`, keyed by the Perl name, with Raku equivalents
attached — not an index of Raku's own routines. Any Raku routine with no Perl 5 counterpart is
absent from it by construction.

`rotor` is the case that surfaced this, while implementing
`todo/tickets/rotor-global-routine-missing.md`:

```
$ grep -c rotor raku-doc/doc/Language/perl-func.rakudoc
0
$ raku -e 'use v6.e.PREVIEW; say rotor(3, 1..6)'
((1 2 3) (4 5 6))
$ raku -e 'say rotor(3, 1..6)'
===SORRY!=== Error while compiling -e
Undeclared routine: rotor
```

`rotor` resolves with **no module loaded** — a genuine core routine, gated on language version
6.e — while appearing zero times in the file the rule named as the authority. Applied literally,
the rule would have rejected a real Raku builtin. `Type/List.rakudoc` documents it as
`multi rotor(**@cycle, \thing, Bool() :$partial)`.

## The replacement test

A function belongs in core only if **both**:

1. `raku -e '<call>'` resolves it with no `use` statement — a `use v6.X` version pragma still
   counts as core, since a version-gated routine is core, not a module; and
2. it is documented under `raku-doc/doc/Type/`, or in
   `raku-doc/doc/Type/independent-routines.rakudoc` (the actual index of routines not tied to a
   single type).

`perl-func.rakudoc` keeps its place as a *migration* reference; it is just no longer the
membership test, and `CLAUDE.md` now says so explicitly so the mistake is not repeated.

## Verified against the cases that matter

| candidate | check 1 (no `use`) | verdict | matches reality |
| --- | --- | --- | --- |
| `rotor` | resolves under `use v6.e.PREVIEW` | core | yes — 6.e core routine |
| `is_run` | `Undeclared routine` | module | yes — `Test::Util` |
| `cglobal` | `Undeclared name` | module | yes — `NativeCall` |

The two July decisions that cited the old rule —
`news/2026-07/nativecall-cglobal-and-native-methods.md` and
`news/2026-07/nativecall-exports-are-module-routines.md` — reached the **right conclusions**;
`cglobal` and the NativeCall exports really are module routines, as the table above confirms
under the new test. Only their cited justification was weak, so those entries are left as the
historical record they are.
