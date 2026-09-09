# A call's scope stacks stopped being `mem::take`n vectors

[ADR-0077](../../docs/adr/0077-locals-are-a-window-into-a-contiguous-stack.md)
moved a call's *slot array* onto one contiguous stack addressed by a per-frame
base, and deliberately left a neighbouring cluster alone so that its own slices
would measure one thing. This is that neighbour, closed:
[ADR-0078](../../docs/adr/0078-per-call-scope-stacks-are-windows-not-moved-vectors.md).

Besides its slots, every call isolates five *scope stacks* from its callee —
`block_declared_vars`, `loop_local_vars`, `loop_local_saved_env`,
`active_loop_param_names`, `active_loop_rw_param_names`. Isolation is not
optional: without it a routine's own `my $x` registers in the **caller's**
active `BlockScope` frame and is reverted at the caller's block exit
(`sub f($n){ my $r=0; { $r=10; f($n-1) if $n>0 }; $r }` returned 0 instead of
10), and the loop-local twin has the same failure through a `while` body.

Every call path bought it with `std::mem::take`: swap an empty `Vec` in, hold
the caller's in a Rust local, assign it back on return. On `fib(22)` that ran a
`Vec::drop` **three times per call** — 171 936 drops for 57 312 calls, 2.21% of
the profile — because assigning the caller's vector back destroys the callee's.
Plus five header moves each way, and one `Vec::clone` per call (1.47%) from a
`saved.clone()` in `vm_call_light` that had no reason to be there: the handle is
moved on all three exit paths now and the compiler accepted it, which is the
proof the clone was never needed.

They are now `ScopeStack<T>`: one shared `Vec` per field, and a `base` marking
where the executing call's frames start. `push_frame()` moves the base up and
hands back a non-`Copy` handle; `pop_frame(handle)` truncates and restores it.
`VmCallFrame`'s five `Vec` fields became five `Option<ScopeFrame>`.

## Measured

Callgrind retired instructions, same tree with the change applied and stashed,
outputs verified identical first — the oracle ADR-0077 had to fall back on after
finding the bench CI cannot resolve a change this size on its runner.

| program | JIT on | | JIT off | |
| --- | ---: | ---: | ---: | ---: |
| `fib(22)` | 147 698 110 → 138 929 481 | **−5.94%** | 253 401 683 → 244 632 361 | **−3.46%** |
| `tak(14,7,0)` | 1 705 079 510 → 1 642 944 324 | **−3.64%** | 2 727 205 630 → 2 665 070 843 | **−2.28%** |
| a `while` loop (control) | 851 658 050 → 851 662 279 | +0.0005% | 1 346 361 430 → 1 346 360 853 | −0.00004% |

`Vec::drop`, `Vec::clone`, `Vec::truncate` and the inlined `raw_vec` all left
the light call path's profile; what replaces them is `scope_stack.rs` inlined
there at 1.73%, ten frame opens and closes per call for about 42 instructions.
The control is a `while` loop with no sub call in it, which the change cannot
touch; it moved by 4 229 instructions out of 851 million, which is what makes
the other rows the call path rather than a code-layout lottery.

**The first version bought less than half of that, and the reason is worth
keeping.** `pop_frame` truncated unconditionally, and `Vec::truncate` promptly
turned up in the profile at 1.61% — it is out-of-line, because it drops a range
of `T`. The common case is a callee that opened no scope frame at all, so
`pop_frame` now tests the length first, exactly as `Locals::push_frame` already
does with `resize`. That one branch is the difference between −3.65% and
−5.94%: replacing a cheap-looking operation with another cheap-looking operation
buys nothing unless the *call* goes away too.

Worth stating what this cluster was **not**: allocation. `fib(22)` performs
20 708 allocations either way and that number does not move at all — a callee's
`mem::take`n vector started at capacity 0 and only allocated if the callee
actually opened a scope frame, which `fib`'s body does not. The win is header
traffic and out-of-line calls, which is why retired instructions rather than an
allocation count are the oracle here.

## Two things worth keeping

**`mem::take` was providing a floor nobody had written down.** Two of the five
stacks pop unconditionally (`self.block_declared_vars.pop().unwrap_or_default()`).
That was harmless while a callee held its own empty vector; against a shared
vector it eats the *caller's* frame. `ScopeStack::pop` is therefore floored at
the base, and `Deref` yields `[base ..]` so `.iter()`, `.last_mut()` and
`.is_empty()` keep meaning "this call's frames". Neither property is defensive
— both are load-bearing, and both are silent when wrong.

**It closes a latent GC-root gap rather than opening one.** `gc_roots` visited
`&self.loop_local_saved_env`, the live field. Under `mem::take`, a suspended
caller's saved loop-local values were sitting in a Rust local *inside*
`call_compiled_function_positional_light_at` — invisible to the root scan for
the whole duration of the call. They are now in the shared vector, and the scan
reads `all_frames()`, so every live call's frames are rooted. (Reading roots
through the `Deref` window instead would have been the mirror-image bug; a unit
test asserts the two disagree in the direction they must.)

## Also found, and filed rather than fixed

Writing the regression test turned up a pre-existing hang that has nothing to do
with this change: a recursive sub that links two of its locals into a reference
cycle never terminates —
[#7697](https://github.com/tokuhirom/mutsu/issues/7697). It reproduces at
recursion depth 2 on `main`, with no output and no error.

The reason nobody had seen it is worth its own note: the hang disappears the
moment a file loads `Test`. `Test` uses `EVAL`, which latches
`reflective_name_access_possible()` on for the rest of the program, and that
flag is what the light call path consults to decide whether to mirror bound
parameters into the env by name. The failing configuration is the *optimized*
one. Since every `t/` file loads `Test`, the whole TAP suite is structurally
blind to it — `t/locals-frame-stack-gc-roots.t` contains the hanging sub
verbatim at depth 60 and passes.
