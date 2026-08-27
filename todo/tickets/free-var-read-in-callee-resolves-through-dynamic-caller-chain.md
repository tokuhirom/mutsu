# A named sub's free variable resolves through the DYNAMIC caller chain, so an intervening caller's same-named local shadows the lexical one

Found while fixing
`todo/deep/bind-propagate-ancestor-frames-clobbers-unrelated-recursive-locals.md`
(now `news/2026-08/bind-propagate-ancestor-frames-frame-ownership-gate.md`).
Independent of that bug and NOT fixed by it — verified unchanged before and
after that fix.

## Symptom

A caller's own `my` lexical is clobbered by a `:=` bind performed in a callee
that names a *different* (mainline) variable with the same name.

```raku
my $var = 1;
my $alias;
sub g() { $alias := $var; }
sub f() { my $var = 5; g(); say "f sees $var"; }
f();
$var = 200;
say "alias $alias";
```

* `raku`:  `f sees 5` / `alias 200`
* `mutsu`: `f sees 1` / `alias 200`

`f`'s own `$var` reads back as `1` after the call to `g` — `f` lost its own
lexical to the mainline one.

## Root cause (partly established)

mutsu's `Env` is a *dynamic* chain: a callee's env is
`Env::scoped_child(caller_env)` (`src/vm/vm_call_named_inner.rs`), so a free
variable read inside `g` walks `g -> f -> mainline` rather than `g`'s lexical
parent (the mainline). Lexical scoping is only approximated by that caller
chain, and it diverges exactly when an intervening caller declares the same
name.

In this particular shape the divergence surfaces through the `:=` bind
machinery — `Interpreter::propagate_bind_to_ancestor_frames`
(`src/vm/vm_var_assign_ops.rs`) writes the bind's shared `ContainerRef` into
the innermost ancestor frame that declares `var` — but the reported value `1`
(the *mainline* value, not `f`'s `5`) shows `f`'s `my $var = 5` did not even
land in `f`'s own env tier: the innermost frame the splice found was the
mainline's, and `f`'s later read of `$var` went to env rather than to its
local slot. So there are (at least) two interacting mechanisms here: the
dynamic-chain resolution, and a local/env dual-store coherence gap for a
routine-level `my` that is never written by name.

## Re-measured 2026-08-28: unchanged by ADR-0055 slice 1, and unchanged by the slice-2 merge flip

The repro was re-run against ADR-0055 slice 1 (the vouch/cell dichotomy — every
escaping-captured plain scalar is now either authoritative or a shared cell) and
against a prototype of slice 2 (the closure-wins merge). Both leave it at
`f sees 1 / alias 200`.

That is the expected result, and it sharpens the scope: ADR-0055 is about how a
*captured env* is merged into a closure's frame, whereas this is about how a
*named sub's* frame is chained to its caller in the first place
(`Env::scoped_child(caller_env)`). A merge policy cannot fix a frame whose
parent is the wrong frame. This needs its own ADR — "a routine's env parent is
its lexical scope, not its caller" — and ADR-0055 §7.5 now records it as
out of scope for that ADR.

## Why this is not a small slice

Making free-variable resolution genuinely lexical (resolve through the
closure's captured env rather than the dynamic caller chain) is an
architectural change to the env model and is very likely ADR territory. The
narrower half — why `f`'s `my $var = 5` is not visible in `f`'s own env tier
when a callee reads the name — may be independently fixable and is the
cheaper thing to investigate first.

## Repro

The snippet above; `raku` is the oracle.
