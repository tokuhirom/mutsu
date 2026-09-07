# A non-constant parameter default still forfeits the light-call path

`todo/perf/defaulted-param-forfeits-the-light-call-path.md` is closed
(`news/2026-09/defaulted-params-reach-the-positional-light-path.md`): a routine
whose optional positionals all reduce to a **constant** — a literal default, or
the type object a bare `?` binds — now reaches `pos_light_call_cache`, and the
binder fills the omitted tail from a table computed once at registration.

That was tiers 1 and 2 of that ticket's plan. Tier 3 is still open, and it is
what the original file called "the general answer but a compiler change, not a
binder one".

## What still misses

Any signature where an omitted parameter's value cannot be a shared constant:

```raku
sub f($x, $y = $x + 1)      { }   # reads an earlier parameter
sub g($x, $y = expensive()) { }   # arbitrary expression
sub h($x, $y = [1, 2])      { }   # must be a FRESH container per call
sub i($x, $y is copy = 1)   { }   # a trait changes what binding means
sub j($x, Bool(Mu) $y = 1)  { }   # a coercion type
```

Each keeps the whole routine on `bind_function_args_values`, so every call
re-resolves it by name through `resolve_function_with_types`, and each *omitted*
argument additionally runs `eval_param_default` — which clones the default's AST
and **compiles it afresh on every call** (`eval_block_value(&[Stmt::Expr(expr.clone())])`).
That second cost is the larger of the two and is not specific to the light path:
it is paid by the general binder for every non-constant default anywhere.

## The shape of a fix

Compile each default expression **once**, into the callee's own prologue, so the
binder never evaluates an expression at all: an omitted parameter jumps to a
per-parameter entry point that runs the default's bytecode against the
already-bound earlier parameters, then falls through. That is what makes
`$y = $x + 1` work on a slot-only path — the earlier parameter is already in its
slot — and it subsumes the container case for free, since the prologue builds a
new container per call the same way the body would.

`is copy` and coercion parameters are a separate axis and should not be folded
into this: they change what *binding* means, not how the default is produced.

## Why it may not be worth it

Measure before building. The constant cases were the common ones, and they are
done. Before starting, count how much of a real corpus is left:

```sh
MUTSU_VM_STATS=1 target/debug/mutsu <program>   # function-full-resolve, by name
```

A name that still shows one resolve per call is a routine this would fix. If the
survey over `t/` and the roast whitelist turns up only a thin tail, close this
file instead — the compiler change is not small (a per-parameter entry point in
the callee prologue, plus the JIT's view of it), and the ticket it came from was
motivated by a `Test`-module claim that has since gone stale (see the news entry
above for that measurement).
