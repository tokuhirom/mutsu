# A block's `return` is classified where the block is written, not where it is called (ADR-0050)

```raku
class C { method m() { "orig" } }
my &w = -> |c { return "R" };
C.^lookup('m').wrap(&w);
say C.new.m;
```

raku dies with `Attempt to return outside of any Routine`: the pointy block is
not a Routine and no Routine encloses it. mutsu printed `R` — the block's
`return` returned from the method it wraps (#9892).

The compiler already classified the block correctly at its definition site.
But `.wrap` (and every other native that calls a code object through
`call_sub_value`) runs a closure body through the block-value carrier, which
recompiles the body — and it re-derived the classification from the call
stack: "some frame is live, so this is a routine". Inside the wrapped method
some frame always is.

ADR-0050 (now Accepted) makes the classification a recorded fact:
`CompiledCode` carries the definition-site `lexically_in_routine` beside its
`is_routine`, the carrier is handed that pair as a parameter, and the carrier
compile cache keys on it. Only a body that no code object owns (a regex code
block, a `where` clause run by name) still gets the dynamic answer, as an
explicitly named fallback. The inline `.map`/`.grep` compile asks its origin
chunk the same way.

Pinned by `t/routines/closure/wrap-block-return-definition-site.t`;
`roast/S04-statements/return.t`, `roast/S06-advanced/return.t` and
`roast/S06-advanced/wrap.t` stay green.
