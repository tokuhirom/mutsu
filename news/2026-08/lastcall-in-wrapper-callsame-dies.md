# `lastcall` inside a method wrapper no longer crashes a following `callsame`

ADR-0019 E9-pre found that `lastcall` inside a method wrapper correctly empties the wrap chain,
but a following `callsame` died with `"callsame is not in the dynamic scope of a dispatcher"`
instead of returning `Nil`:

```raku
class C { method m() { say "orig"; "o" } }
C.^lookup('m').wrap(-> |c { say "wrap"; lastcall; my $r = callsame; say "after({$r // 'Nil'})"; "w" });
say C.new.m;
# raku:            wrap -> after(Nil) -> w   (lastcall empties the chain; callsame yields Nil; orig never runs)
# mutsu (before):  wrap -> dies "callsame is not in the dynamic scope of a dispatcher"
```

## Root cause

`lastcall` (`builtin_lastcall`, `src/runtime/builtins_dispatch_next.rs`) correctly empties the
wrap frame's `remaining` list without destroying the frame. The bug was further down:
`dispatch_next_candidate`, when it finds an exhausted method-wrap frame (`sub_id == 0`, `remaining`
empty), falls through so a `callsame` inside the *original* method can continue the MRO chain —
but a plain (non-multi, non-inherited) wrapped method pushes **no** `method_dispatch_stack` frame
at all, since there is nothing left in its MRO to defer to. The function's final fallback,
`if !self.method_class_stack.is_empty() { Nil } else { no_dispatcher_error }`, had no way to know
it had arrived here via an exhausted wrap chain (which *is* a live dispatch context) rather than
a genuinely dispatcher-less call, so it took the error branch.

## Fix

A new `wrap_chain_exhausted` flag is set exactly when the wrap-frame fallthrough happens, and the
final fallback now also returns `Nil` when it is set:
`if !self.method_class_stack.is_empty() || wrap_chain_exhausted`.

Pinned by `t/lastcall-in-wrapper-callsame-dies.t`, verified against both raku and mutsu. The full
`lastcall`/`nextsame`/`callsame`/`wrap`/`samewith`/`callwith` regression corner of `t/` stays
green.

## Scope note

The sibling `nextsame` (tail-call) variant of this same scenario turned up a further, unrelated
divergence in raku's own output — filed separately as
`todo/tickets/lastcall-in-wrapper-nextsame-swallows-output.md`, not fixed here.
