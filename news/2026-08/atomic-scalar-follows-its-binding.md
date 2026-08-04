# An atomic scalar follows its binding, not its name

`my atomicint $i` kept its value in a **process-global store keyed by the bare
variable name** (`__mutsu_atomic_name::i` → `__mutsu_atomic_value::N`). Since
every scalar declaration clears the entry for its own name
(`reset_atomic_var_key_decl`), an unrelated `my $i` anywhere else in the program
silently reset the counter:

```raku
sub unrelated() { my $i = -1; $i }

my atomicint $i = 0;
say ++⚛$i;          # 1
unrelated();
say ⚛$i;            # mutsu: 0     raku: 1
```

A bare `my $i;` with no initializer was enough. The lane has no binding
identity, so two `$i`s anywhere in a program are one atomic variable.

## Fix: the shared cell is the atomic primitive

`cas` already preferred a `ContainerRef` cell over the lane when the closure
machinery had boxed the target (`scalar_cell_target`). The other atomic scalar
primitives — `⚛`, `⚛=`, `⚛+=`, `⚛++`/`⚛--`, `++⚛`/`--⚛` — now do the same, and
`atomic_scalar_cell` boxes the binding into a cell on first atomic touch when
the running frame owns it. The cell's mutex is the atomic primitive; every alias
of the binding, including a spawned thread's clone, holds the same cell, so no
`shared_vars` side channel is involved and two same-named bindings cannot
collide. A name that already had a lane entry seeds its cell from that value and
retires the entry, so a `cas`-then-`⚛` sequence keeps one source of truth.

## The bump had to count as a write first

`box_captured_lexicals` only boxes a local a closure both captures **and**
mutates, and `++⚛$c` compiles to a `__mutsu_atomic_pre_inc_var("c")` *call* — no
name-write opcode, so the free-variable analysis saw a read-only capture and
declined to box. A closure that does nothing but bump a captured `atomicint`
therefore never got a cell. `CompiledCode::atomic_target_syms` now records every
name that reaches an atomic builtin as its target, and `compute_free_vars` folds
those into `free_var_writes` / `self_mutated`.

That is what a Cro request counter needs:

```raku
my $application = route {
    my atomicint $i = 0;
    get -> 'counter' { content 'text/plain', (++⚛$i).Str }
}
```

which answered `0` on every request — the route block's `$i` was reset by a
`my $i` inside `Cro::HTTP::Router::LinkGenerator` — and now counts. It is the
`http-middleware.rakutest` subtest 4 shape.

Pin: `t/atomic-scalar-follows-its-binding.t`.
