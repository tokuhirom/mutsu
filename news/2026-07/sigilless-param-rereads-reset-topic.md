# A sigilless parameter no longer re-reads the callee's freshly-reset `$_`

Passing the topic to a routine with a sigilless parameter bound the *type object*
instead of the value:

```raku
class K { method m (K:D: Int \ch = 1) { "ch={ch}" } }
$_ = 7;
K.new.m($_);
# raku:  ch=7
# mutsu: X::TypeCheck::Binding::Parameter: Type check failed for ch: expected Int, got Package
```

An untyped `\ch` was worse — it bound `Any` silently, so the body just saw an
uninitialized value. A sigiled `Int $ch` was already correct, which is what made
the bug look arbitrary.

Found in `String::Rotate` (`TODO_dist` T-057), whose test does
`for ^$str.chars { $str.rotate($_) }` against
`multi method rotate (Str:D: Int \ch = 1)` — the whole file died at the first
call.

## Root cause

A sigilless parameter is bound by re-reading its argument from the **callee's**
env under the argument's source name, so a later `x := …` can write through to
the caller's variable. `binding_signature.rs` already excluded the compile-time
pseudo-variables from that reread, for exactly the right reason:

> Compile-time pseudo-variables (?CLASS, ?ROLE, ?PACKAGE, etc.) should NOT be
> re-resolved from the callee's env because the env may have already overwritten
> them.

The per-routine magic names are the same hazard and were missing. By the time
binding runs, the frame has already reset `$_` to `Any` and `$!` to `Nil`, and
rebound `@_`/`%_` to its own arguments. So `f($_)` re-read the freshly-blanked
`_` and bound `Any`.

The reason it looked like it depended on the invocant smiley (`K:D:` failed,
`K:` worked) is that the source-name list only lines up with the positional index
in some signature shapes; where it did not line up, the reread was skipped and
the passed value survived by accident.

## Fix

The reread now skips `_`, `!`, `@_` and `%_` alongside the `?`-prefixed
pseudo-variables, using the argument value as passed. Ordinary lexicals are
untouched, so `sub writes (\x) { x = 99 }` still aliases and writes through.

`String::Rotate`'s method half now passes. Its `sub rotate` half is a separate,
broader root cause — a user sub that shadows a builtin loses the call when it has
a default parameter — recorded in
`todo/tickets/builtin-shadow-with-default-param-loses.md`.

Pin: `t/sigilless-param-magic-source.t` (12 tests, identical output under `raku`)
— the topic through every invocant shape, from a `for` loop and a `given`, `$!`
as an argument, the write-through alias that must keep working, and the
`augment` + role shape from the dist.
