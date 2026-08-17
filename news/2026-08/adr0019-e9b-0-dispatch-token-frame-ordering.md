# callsame/nextsame now resolve to the innermost dispatch context, not a fixed wrap-then-method-then-multi order

`callsame`/`nextsame`/`callwith`/`nextwith`/`lastcall`/`nextcallee` pick their
deferral frame by walking three independent stacks — sub wraps, method MRO
chains, multi candidate lists — in a fixed priority order: wrap, then method,
then multi. That priority is wrong whenever one stack's frame is nested
*inside* a call on another stack, because the outer frame stays on top of its
own stack the whole time the inner call runs:

```raku
class P { method m() { "P-m" } }
class C is P { method m() { "C-m[" ~ callsame() ~ "]" } }
sub g() { "g-orig" }
&g.wrap(sub () { say C.new.m; "g-wrap[" ~ callsame() ~ "]" });
say g();
# raku:  C-m[P-m]    then g-wrap[g-orig]
# mutsu (before): C-m[g-orig] then "Use of Nil in string context" + g-wrap[]
```

`C.m`'s `callsame` should walk `C`'s MRO and reach `P.m`. Instead mutsu found
`g`'s still-live wrap frame first (it's always checked first) and re-ran `g`'s
original body; `g`'s own subsequent `callsame` then found an already-exhausted
chain and returned Nil.

## Fix

`wrap_dispatch_stack`, `method_dispatch_stack`, and `multi_dispatch_stack`
frames each now carry a `dispatch_token: u64` stamped from one shared
monotonic counter (`Interpreter::next_dispatch_token`) at push time. The three
selection sites (`dispatch_next_candidate`, `builtin_lastcall`,
`builtin_nextcallee`) compare the top frame's token across all three stacks
and act on whichever is highest — the innermost live dynamic dispatch context
— via a new `innermost_dispatch_stack()` helper, instead of the fixed search
order.

Today's paired method-wrap frames (a wrapped method call pushes a method
frame, then a wrap frame for its own wrapper chain) are unaffected: the wrap
frame is still pushed second, so it still wins the innermost check, and its
sentinel-exhaustion fallthrough to the paired method frame is preserved
explicitly. Only genuine cross-stack nesting — a method deferral inside a sub
wrapper, or vice versa — changes behavior, which is exactly the bug above.

ADR-0019 E9b-0 (see `news/2026-08/adr0019-e8-e11-candidate-sequence-semantics.md`,
"E9b design" § decision 4). Found and raku-confirmed during the E9b design
pass; fixes
`todo/tickets/callsame-in-method-consumes-enclosing-sub-wrap-chain.md`.
Pinned by `t/dispatch-token-cross-stack-nesting.t` (both nesting directions,
verified green under `raku` first).

The companion divergence found in the same design pass — a wrap chain
suppressed by an unrelated *foreign* wrap dispatch's global guard
(`wrap-chain-skipped-inside-foreign-wrap-dispatch.md`) — is untouched here;
that one requires deleting the guard as part of E9b-2's single-frame cutover,
not just a priority fix.
