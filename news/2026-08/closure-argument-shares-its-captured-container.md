# A closure passed as an argument shares its captured container

Two closures created over the same `my $c` disagreed about its value as soon as
one of them was passed to a routine:

```raku
my @handlers;
sub register(&h) { @handlers.push: &h }

{
    my $c = 0;
    register { $c = $c + 1 };
    register { $c };
}

@handlers[0]();
say @handlers[1]();   # mutsu: 0        raku: 1
```

The bumper only appeared to work because its own env snapshot was written back
to itself; every other view of `$c` — a sibling closure, the declaring scope, a
later call on another thread — kept the value captured at creation time. Calling
the stored handler from a spawned thread restarted the count from `0` on every
request, which is exactly what a Cro request counter looks like:

```raku
my $application = route {
    my $i = 0;
    get -> 'counter' { content 'text/plain', (++$i).Str }   # answered 0 forever
}
```

## Cause: the escape analysis classified every call argument as non-escaping

A captured-and-mutated lexical is promoted to a shared `ContainerRef` cell only
when some child closure's value **escapes** the creating frame
(`CompiledCode::needs_cell_locals`, driven by the compiler's `escaping_position`
flag). Assignment RHS, `return` operands, block tails and literal elements were
escaping positions; call and method arguments were not, on the theory that an
argument is handed to the callee rather than stored in the caller frame. Only an
allowlist — `start` for functions, `then`/`tap`/`act`/`start` for methods — opted
back in.

That theory is wrong: the *callee* decides whether the closure is invoked
immediately or stored, and the caller cannot know which. `register { ... }` keeps
it alive indefinitely, so `$c` needed a cell and never got one.

## Fix

A closure argument is now unconditionally compiled in an escaping position, for
both function calls (`compile_call_arg_with_escape`) and method calls
(`method_escapes_closure_args`). `thread_escaping` — the strictly narrower signal
that relaxes the typed-scalar boxing skip — stays limited to `start`.

The allowlist existed as a boxing-cost guard (#2746). Measured on 2026-08-04, it
bought nothing. Boxing is only ever considered for a local that a child closure
both captures **and** mutates, so the overwhelmingly common `map {...}` /
`lives-ok {...}` / `grep {...}` argument closes over nothing mutable and is
untouched. The one shape that does box — an accumulator mutated from a block
argument — got *faster*, because a shared cell replaces the by-name env
writeback that used to keep the snapshot coherent:

```
200k-element map with an accumulator + 500k `apply({ $n = $n + $^v }, $i)`
  before: 8.22 / 8.30 / 7.96 s
  after:  7.91 / 7.60 / 7.45 s
```

The micro-benchmark set (`fib`, `int-arith`, `method-call`, `poly-call`,
`hash-access`, `word-count`, `array-ops`, `string-concat`, `num-arith`,
`mandelbrot`, `tak`) showed no change beyond run-to-run noise.

Pin: `t/closure-arg-shares-its-captured-container.t`, which covers the sibling
closure (function and method argument) and the cross-thread accumulation the Cro
counter needs.
