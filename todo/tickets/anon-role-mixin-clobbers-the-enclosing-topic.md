# An anonymous-role `but` mixin clobbers the enclosing `$_`

Found 2026-08-03 while writing a GC stress script for the `Mixin` overrides-map migration
(`news/2026-08/mixin-overrides-map-is-a-gc-node.md`). Pre-existing — confirmed by stashing that
change and rebuilding; unrelated to it.

## Repro

```
$ mutsu -e 'for ^3 { my $o = 42 but role :: { method z { 1 } }; say "n=$_"; }'
Use of uninitialized value element of type __ANON_ROLE_0__ in string context.
Methods .^name, .raku, .gist, or .say can be used to stringify it to something meaningful.
  in block <unit> at -e line 1
n=
... (x3)

$ raku -e 'for ^3 { my $o = 42 but role :: { method z { 1 } }; say "n=$_"; }'
n=0
n=1
n=2
```

Evaluating `but role :: { ... }` inside a `for` body replaces the loop topic with an
uninitialized instance of the anonymous role type. Every later `$_` in that iteration sees the
wrong value: interpolation yields the empty string plus a spurious uninitialized-value warning,
and anything that actually uses the topic gets the role type object.

Without the mixin the same loop prints `n=0 n=1 n=2`, so it is the mixin evaluation — not the
loop or the string interpolation — that writes the topic.

## Why it is worth a ticket rather than a quick fix

This looks like the same *family* as the role-parameterisation topic leak from 2026-07-29 (the
one whose debugging story is written up in CLAUDE.md's `rust-gdb` section: the topic's env key is
`"_"` with no sigil, and only `SetTopic` writes it). That one was fixed; this path is not
covered, so the fix is presumably a second site that composes an anonymous role and leaves a
value in the topic slot — most likely the role-composition/`but`-mixin path setting up a
`self`/topic for the role body and not restoring it.

Start by breaking on the `SetTopic` / `RestoreTopic` arms in `vm/vm_exec_dispatch.rs` with the
repro above and reading who writes `"_"` during `but role :: {...}` — per the debugging
guidelines, do not guess a key name and spend a rebuild on it.

A pin belongs in `t/` once fixed: the topic must survive a mixin in a loop body, and
`$_` must still be the loop value afterwards.
