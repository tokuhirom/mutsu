# A short call to a pointy block reports "Too few positionals", not an undeclared variable

```raku
my $b = -> $a { $a };
say (try { $b() } // $!.^name ~ ': ' ~ $!.message);
```

`raku` answers `X::AdHoc: Too few positionals passed; expected 1 argument but
got 0`. mutsu answered `X::Undeclared: Variable '$a' is not declared` --
the *body*'s attempt to read the never-bound `$a`, not the bind rejecting
the call.

A single-parameter pointy block (and a non-mutating `WhateverCode`) carries
no `ParamDef`: its signature survives only as the plain parameter name in
`SubData::params`, taking the general binder's legacy
`param_defs.is_empty()` branch (`src/runtime/types/binding_signature.rs`).
That branch's per-parameter loop already raised "Too few positionals passed"
for a `^`-twigil placeholder param running out of arguments, but a plain
positional identifier (`-> $a { … }`, stored sigil-less as `"a"`) simply fell
off the end of the loop with nothing bound -- the surplus direction was
already guarded (the `all_plain_positional` check that raises "Too many
positionals passed"), only the short direction was missing.

The fix adds the same check in the short direction, computed before the
bind loop runs (rather than discovered mid-loop) so a short call rejects
atomically instead of partially binding whatever arguments the caller did
supply. While in there, the "too few" messages (both the pre-existing
`^`-placeholder one and the new plain-positional one) also pick up the
pluralized `argument`/`arguments` wording the "too many" message next to it
already had -- `raku` says "expected 1 argument" (singular) but "expected 2
arguments" (plural), which the old, always-plural text did not match.

`t/routines/signature/too-few-positionals-fixed-arity.t` pins the shapes
this touches, mirroring `too-many-positionals-fixed-arity.t`'s coverage in
the short direction; `t/routines/closure/pointy-block-light-bind.t`'s arity
assertion, previously only checking that a short call dies at all, now
checks the exact message too.

A first version of the check compared against `positional_args` directly and
regressed `.sort(-*.value)` over a `Hash`/`Bag`/`Mix`: a bare WhateverCode's
`params` is the sentinel `["_"]`, which reads its implicit argument through
the dynamically-scoped topic `$_` rather than a real positional bind, so a
Pair-shaped element (promoted to a `ValuePair` by `pair_as_positional`) is
deliberately excluded from `positional_args` for that shape and was
miscounted as "0 positionals supplied" -- rejecting every per-element call
and collapsing every sort key to the same fallback value, non-deterministically
scrambling the result order (`.sort`'s hash-iteration order varies by run).
The check now compares against a separate `arity_positional_count` that
still counts a `ValuePair` regardless of that promotion rule, so a genuinely
empty call (`(* + 1)()`) is still caught while a `.sort`-fed Pair argument is
not. `t/routines/closure/whatevercode-pair-arg-arity.t` pins this interaction.

[#8353](https://github.com/tokuhirom/mutsu/issues/8353)
