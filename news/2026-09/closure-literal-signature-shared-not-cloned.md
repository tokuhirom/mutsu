# A closure literal's signature is shared, not deep-cloned per creation

`SubData::body` stopped being a `Vec<Stmt>` in the Part C slice of
[#7557](https://github.com/tokuhirom/mutsu/issues/7557): the block's AST is
pool-owned and immutable, so every closure created from one literal shares one
`Arc` instead of deep-cloning the body on each creation. The signature — the
other pool-owned, immutable half of a closure literal — was left behind, and
that is what this change fixes.

## The cost

`SubData::params` was a `Vec<String>` and `SubData::param_defs` a
`Vec<ParamDef>`, both cloned out of the `stmt_pool` on every closure creation.
`ParamDef` is fat: a `String` name, an `Option<Expr>` default, an
`Option<Vec<ParamDef>>` sub-signature, an `Option<Box<Expr>>` where-constraint
and a `Vec<String>` of traits. Cloning one is several allocations, and a
creation cloned one per parameter — on the path that runs once per
`.map({...})` / `.grep({...})` / callback-literal evaluation.

The scaling is easy to see by varying only the parameter count. Two loops
building 100000 pointy blocks, one with a single parameter and one with eight,
counted with callgrind (deterministic, so the container's four noisy cores do
not matter):

| | 1 parameter | 8 parameters | difference |
|---|---|---|---|
| before | 2,086,890,637 | 2,859,420,599 | +772,529,962 |
| after | 2,025,362,566 | 2,038,812,420 | +13,449,854 |

Seven extra parameters used to add **~1100 instructions to every single
creation**; they now add ~19. Per creation (control loop subtracted), the
8-parameter literal went from 24,820 to 16,687 instructions, **−32.8%**.

A bare block pays a second, related cost: its `params` are the implicit
placeholder variables (`$^a`, `@_`, …), which `collect_placeholders_shallow`
derives by walking *and sorting* the body. That is a pure function of the pool
entry too, and it ran on every creation.

## The change

`SubData::params` is now `Arc<Vec<String>>` and `param_defs` is
`Arc<Vec<ParamDef>>`, and `CompiledCode::closure_signature(idx)` builds the pair
once per `stmt_pool` slot behind a `OnceLock` — the exact shape
`closure_body_arc` already uses for the body, including its fallback for a chunk
whose pool grew after the side table was sized. The cached entry for a
`Stmt::Block` slot carries the placeholder scan, so that walk-and-sort is now
also paid once per literal rather than once per creation.

All four closure-creation opcodes (`MakeLambda`, `MakeAnonSub`,
`MakeAnonSubParams`, `MakeBlockClosure`) take their signature from there.
`MakeBlockClosure` deliberately does not: a block closure takes no signature at
all, and asks for `value::empty_params()` / `value::empty_param_defs()` — one
shared empty `Arc` per thread, because `Arc::new(Vec::new())` still allocates a
control block per creation, which is precisely the allocation sharing the field
was meant to remove.

Nothing mutates a `SubData`'s signature after construction, which is what makes
the sharing sound. The places that *appear* to (`.assuming` building a primed
signature, `.^add_multi_method` copying a signature into a `MethodDef`,
`callable_signature` handing an owned pair to callers that edit it) take an
owned copy explicitly, exactly as they did before — they simply say so now.

## Effect on the other numbers

| repro | before | after | |
|---|---|---|---|
| `my $c = * + 1;` × 200000 | 4,741,856,528 | 4,548,975,732 | −4.1% |
| `-> $a { $a }` × 100000 | 2,086,890,637 | 2,025,362,566 | −2.9% |
| `-> $a … $h { $a }` × 100000 | 2,859,420,599 | 2,038,812,420 | −28.7% |
| `-> $a, $b, Int $c = 3, :$named, *@rest {…}` × 100000 | 2,640,465,916 | 2,035,588,516 | −22.9% |

A one-parameter `WhateverCode` was never where the signature clone hurt, so its
few percent is the floor; the win grows with the signature, which is what
"O(signature) per creation" meant.

## What is still open on #7557

The kept-set narrowing that Part B actually asks for. `capture_closure_env`
keeps every env key `is_plain_user_lexical` rejects, which is over-broad by
construction; #7624 memoized the resulting `Env` one entry deep, but a capture
that *misses* that memo still pays O(kept env). That needs the design pass the
issue describes — mostly a question about the built-in dynamics — and is not a
drive-by.

Pinned by `t/closure-shared-signature.t`, which checks that closures built from
one literal in a loop keep independent behaviour, and that priming one with
`.assuming` leaves its siblings' arity, defaults and parameter list untouched.
