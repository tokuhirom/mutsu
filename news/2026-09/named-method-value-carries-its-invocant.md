# A named `my method` value carries its invocant

`&m` for a `my method m($x) { ... }` is a `Method`, so it takes the receiver as its first argument.
mutsu registered it with only the *declared* parameters while compiling its body against a leading
`self` — so the registered signature and the compiled bytecode disagreed by a slot.

```
                       rakudo              mutsu (before)
&m.arity               2                   1
&m.signature           (Mu $:: $x, *%_)    ($x)
&m(Obj, 1)             runs                Too many positionals
Obj.&m(1)              runs                Too many positionals
&m(1)                  Too few positionals ran, or died on `self`
```

`Obj.&m(1)` is the ordinary way to apply a method value to an invocant, and it was the form that did
not work — most of the reason to declare a `my method` rather than a `my sub`.

## Root cause

`Stmt::MethodDecl`'s lowering in `src/compiler/stmt.rs` compiles the body against
`["self", "__ANON_STATE__", "?CLASS", "?ROLE", ...params]` a few lines after emitting
`RegisterDecl` — but the plan it registers carried `params` alone. The `&name` value that
`register_sub` builds from that plan therefore had no invocant to bind, and a body mentioning `self`
died with *"Variable '$self' is not declared"*.

The rule already existed: a class body's `our method` registers its sub form through
`method_sub_form_params`, which prepends `self` for exactly this reason. The lowering now uses that
same helper, so both paths agree on what a named method value's signature is. Its leading parameter
is then marked `is_invocant`, because the helper deliberately builds a *sub* shape (where the
invocant is an ordinary first positional) and a `my method` is a `Method`.

## Only the `my` / `our` spellings

[#8348](https://github.com/tokuhirom/mutsu/issues/8348) carried this note over from #8313, and it is
load-bearing: a bare `method foo { }` at unit or inner scope is `===SORRY!===` in rakudo, so it has
no reference signature to match, while mutsu accepts it. The first attempt applied the change to
every `MethodDecl` and broke two tests that call such a nested method with no arguments
(`t/oo/method/nested-method-captured-writeback-coherence.t`,
`t/oo/method/methods-instance-regressions.t`) — a spelling this ticket has no baseline for and no
business changing. The change is gated on `is_my || is_our`.

## Pin

The assertions join `t/oo/method/named-method-declaration-keeps-its-declarator.t`, which #8313 left
with a comment saying why they were absent — 19 tests now, green under mutsu and under real Rakudo:
`.arity`, all three call shapes, the missing-receiver arity error, the `submethod` and `our`
spellings, and a body that actually reads `self`.

## One thing still differs

`&m.signature.gist` is `($self:: $x)` here against rakudo's `(Mu $:: $x, *%_)`. The invocant now
renders as one, which it did not before, but mutsu names it and omits the implicit `*%_`, where
rakudo shows an anonymous `Mu`-typed invocant. Every *behavioural* consumer — `.arity`, `.count`, and
all three call shapes — matches. A class-body method already renders `(C $:: $x, *%_)` correctly, so
the convention exists; closing the last of the gap is a rendering change in a shared helper and is
not worth destabilising this one for.
