# One scalar `:=` no longer taxes every element store in the frame

`benchmarks/bench-threads-serial.raku` was the worst row in `bench-history.tsv` — 1.57x rakudo,
1.46x with the JIT on, where every other benchmark sat at 0.04-1.34. Profiling it found that the
row is not about threads (it has none) and not really about containers either. It is about one
line: `my $sref := $seed`.

`try_fast_array_element_assign` — the lane #8107 and #8151 built to take a plain `@a[$i] = $v`
from ~13,000 instructions to ~1,210 — opened with a refusal keyed on `local_bind_pairs` being
non-empty at all:

```rust
// A `:=` binding in scope can make any name an alias; the slow path owns
// that resolution.
if !self.local_bind_pairs.is_empty() {
    return None;
}
```

That field is **frame-global**. One `my $x := $y` anywhere in a scope pushed every `@a[$i] = $v`
in that scope back onto the full name-keyed store path, including stores into arrays the binding
cannot reach. The rounds that built the lane measured with
`loop (my $i = 0; ...) { @c[$i +& 15] = $i }`, which has no `:=` anywhere, so the lane always
committed and this never surfaced.

The cost was the whole of the regression #8069 §2 was originally filed with, intact for any
program containing a scalar `:=`: 23 `Symbol::intern` calls and 8 heap allocations per store.

## Asking the question per name rather than per frame

A bind pair is a pair of local *slots*, and the only thing the VM does with one is copy a scalar
store's value from the source slot into the alias slot (`exec_set_local_op_inner`'s
reverse-propagation loop, `exec_compound_assign_scalar_for`'s forward one). An element store
writes no local slot at all — it mutates the backing node in place, which every holder of that
node observes — so a pair can only matter when one of its two slots *is* this array.

`bind_pair_names_array` asks exactly that. Most pairs cannot be it: a whole-container `:=`
(`my @b := @a`) is served by a shared `ContainerCell` and returns before any pair is recorded,
which is why the pairs are overwhelmingly scalar-only. They are not exclusively so — an
`@`/`%`/`&` bind routed through `SetGlobal`, i.e. a free-variable `:=` inside a named sub, skips
the cell branch and reaches `resolve_pending_alias_binds`, which records the pair bidirectionally
on two `@` slots — so the gate asks rather than assumes.

Matching by name rather than by slot index is deliberate. A name can occupy several `code.locals`
slots while the lane's own `target_slot` resolves to just one of them, and a frame torn down by an
exception can leave a pair whose slot index does not address this frame at all. Both cases resolve
to "decline", which is always safe.

This is the same narrowing `slot_is_bind_pair_source` already made for the *scalar* store, against
the same field and the same benchmark — its doc comment names `bench-threads-serial.raku` and
records that one `my $sref := $seed` made a bare `$s = $s + ($i +& 255)` loop run 1.87x slower.
The element store was the half that was left behind.

## Measured

Deterministic instruction counts (`--profile profiling`, `MUTSU_JIT=on`, callgrind), over 16,000
element stores. `w0` is the store loop alone; `w2` is the same loop with two extra lines
declaring an unrelated scalar `:=`.

| | before | after |
| --- | ---: | ---: |
| `w0` (no `:=`) | 125,583,516 | 125,675,462 |
| `w2` (unrelated scalar `:=`) | 360,187,226 | **126,349,768** |
| per store, `w2` | 15,858 Ir | **~1,214 Ir** |
| `Symbol::intern` per store, `w2` | 23 | **0** |
| heap allocations per store, `w2` | 8 | **0** |
| `bench-threads-serial` (1/5 scale) | 896,088,313 | **660,271,702** |

`w2` now costs what `w0` costs — the 0.5% residual is the two extra declarations themselves. That
`w0` is unchanged is the regression check: the lane's behaviour for a program with no binding at
all is untouched.

## What this is not

It is not #8069 §4.1. The lane is still a fast path *beside* the name-keyed one, which is the
shape that ADR-0097 exists to replace; this only restores the lane's intended coverage to programs
that happen to contain a `:=`. Nor does it touch the lane's other stand-down: once a second VM
mutator thread exists, the lane still declines for the whole process, so `bench-threads` (the
concurrent twin) is unmoved.

Pinned by `t/vm/writeback/scalar-bind-unrelated-element-store-semantics.t`, the element-store twin
of the existing scalar-store file: 38 assertions covering plain and bound arrays, autovivification,
typed/`is default`/shaped arrays, element-level `:=`, the store's own rvalue, and a
self-referential store — all in a file that has a live scalar binding in scope.

Its section 4 covers the one route that records a pair on `@` slots, and asserts only the
container-independent half: mutsu copies instead of aliasing there, which is a real divergence from
rakudo found while establishing that the route exists at all. Filed separately as
[#8759](https://github.com/tokuhirom/mutsu/issues/8759); restore the aliasing assertions when it
is fixed.

Two sibling findings from the same profiling session are open and untouched here:
[#8748](https://github.com/tokuhirom/mutsu/issues/8748) (one `ContainerRef` cell anywhere disables
the `GetLocal` fast path and the JIT's inline local read for the whole process, +21.7% on a loop
that touches no container) and [#8749](https://github.com/tokuhirom/mutsu/issues/8749) (a celled
local read derefs the cell twice and re-interns the variable name on every read).
