# The per-call locals frame is a pooled `Vec`, not a register window

## Finding

After the September 2026 call-path sweep (`news/2026-09/*`, PRs #7275-#7280 —
`&return` latch, type tags, readonly cache, inlined guards, itemization plan,
payload-free clone/drop), the largest single remaining cluster on
`bench-fib`'s profile is the machinery that gives each call its `locals`
vector:

| symbol | self time |
| --- | ---: |
| `recycle_locals` | 2.36% |
| `<Vec<T,A> as Drop>::drop` | 0.96% |
| `Vec::extend_with` | 0.90% |
| `drop_in_place<[Value]>` | 0.85% |
| `Vec::resize` | 0.68% |

That is **~5.7% of the profile to manage a one-element vector** — `fib`'s only
local is its parameter `$n`. Per call the VM does:

1. `take_locals_from_pool(n)`: pop a `Vec<Value>` off `locals_pool`, `clear()`
   it, `resize(n, Value::NIL)` (an out-of-line `extend_with`);
2. `std::mem::take(&mut self.locals)` to stash the caller's vector, and the
   symmetric restore on return;
3. `recycle_locals(used)`: `clear()` (dropping the callee's slot values) and
   push the vector back onto the pool.

Every step is a 24-byte `Vec` header move plus a heap-backed length/capacity
dance, for storage whose lifetime is exactly LIFO.

## The shape of the fix

The obvious representation is the one every register VM uses: a single
contiguous `locals_stack: Vec<Value>` with a per-frame base index. A call
extends it by `num_locals` (amortized O(1), no pool, no allocation), and a
return truncates it. `mem::take`/restore disappears — the caller's slots are
simply below the callee's base. So does `recycle_locals`, the pool, and the
`Vec` header traffic.

## Why this is not a small ticket

`self.locals` is read or written at **484 sites across 60 files**, almost all of
them as `self.locals[i]`, and several of them depend on the *ownership* the
current representation gives them:

- `std::mem::take(&mut self.locals)` is load-bearing in
  `call_compiled_function_positional_light_at`, `push_call_frame`, and the
  closure/thread capture paths — they hand the vector to somebody else, which a
  window into a shared stack cannot do without copying.
- `VmCallFrame::saved_locals` stores a whole `Vec<Value>` per frame.
- The JIT (`vm_jit_layout.rs`) knows the offset of `Interpreter::locals` and
  emits native code against it, so the layout change is not confined to Rust
  call sites.
- Anything that can reallocate the shared stack invalidates a raw base pointer,
  so the window must be an index, and every borrow of two frames at once
  (caller slot + callee slot) has to be re-expressed.

That is an ADR-class change, not a slice: it should be written up as a
`Proposed` ADR (representation, the migration order for the 484 sites, and what
happens to `VmCallFrame::saved_locals` and the JIT layout) before any code.

## Repro / measurement

```
cargo build --profile profiling
taskset -c 2 perf record -F 4999 -g -o tmp/p.data ./target/profiling/mutsu tmp/fib30.raku
perf report -i tmp/p.data --no-children -g none --percent-limit 0.6 --stdio
```

where `tmp/fib30.raku` is `sub fib(Int $n --> Int) { if $n <= 1 { return $n }; return fib($n-1) + fib($n-2) }; say fib(30);`.

Measure any candidate the way the September sweep did: a temporary same-binary
env switch for the logic, and a cross-build **retired-instruction** comparison
for anything that changes inlining or code size (see
`news/2026-09/payload-free-nanbox-kinds-skip-payload-op.md` for why the two
answer different questions).
