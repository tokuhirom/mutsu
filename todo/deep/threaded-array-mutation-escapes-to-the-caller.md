# Once a thread has run, a routine-local `my @a` mutated from a nested sub is the caller's `@a`

After *any* thread has run in the process, a routine's own `my @a` stops being
private: a mutating array method called on it from inside a nested `sub` writes
through to the caller's same-named lexical.

```raku
sub inner-push() {
    my @arr;
    sub bump() { push @arr, 'x' }    # nested sub closing over @arr
    bump();
    @arr.join(",")
}

my @arr = <SENTINEL>;
await start { 1 };                   # any thread at all arms the shared lane
say inner-push();                    # x          -- right
say @arr.join(",");                  # x          -- WRONG, raku says SENTINEL
```

Without the `await start { 1 }` the same program is correct, so the trigger is
`shared_vars_active`, which never returns to false once set
(`src/runtime/runtime_thread.rs:193`).

## What exactly leaks

Measured with `tmp/nb16.p6` (matrix of sigil × mutation, all after one thread):

| callee does, from inside a nested sub | leaks to caller? |
|---|---|
| `push @arr, 'x'` (listop)   | **yes** |
| `@arr.push('x')` (method)   | **yes** |
| `@arr = ('x',)` (assignment)| no |
| read only                   | no |
| `%h<k> = 'x'`               | no |
| `$s = 'x'`                  | no |

So it is specifically the **array mutating-method receiver**, resolved by name,
and only when a nested closure makes `@arr` a captured/free variable — a nested
`for` / `if` / bare block does not do it (`tmp/nb14.p6`).

## Why it is where it is

This is the array half of
[`module-file-scope-array-and-hash-still-share-the-caller.md`](../tickets/module-file-scope-array-and-hash-still-share-the-caller.md),
seen from a different angle. Scalars are isolated under the shared lane by
`thread_redeclared_vars`, which `exec_set_var_dynamic_op`
(`src/vm/vm_var_assign_set_local.rs:1910`) and the parameter-binding equivalent
(`src/vm/vm_call_named_inner.rs:213`) both populate — and both deliberately skip
`@`/`%`, because those names back the name-keyed atomic element stores that
concurrent `push`/element-assign need maintained. A container has no single
by-name chokepoint the way a scalar does: the mutating-method receiver is
resolved straight out of `self.env` in `call_method_mut_with_values`
(`src/runtime/methods_mut_dispatch.rs`) and in the ~20 `env_mut().get_mut(name)`
sites across `src/vm/vm_var_*.rs`, so there is nowhere to hang the isolation.

Fixing it properly is the same chokepoint consolidation ADR-0001 fuses with GC
("layer 3a": container-kind variants become `Gc<T>` and element cells become
`ContainerRef`), which is why the older ticket parks it there too.

## Why it matters now

It is the top blocker for the vendored Cro::HTTP suite's round-trip files. Every
test that constructs a second `Cro::HTTP::Server` dies with

```
Components controlled by a connection manager must compose to form a transform or a sink
```

because `Cro.compose` (`Cro.rakumod`) has a method-local `my @components` that a
nested `sub push-component` pushes to, and `Cro::ConnectionManager.BUILD` takes a
`:@components` named parameter. Once the first request has run (threads active),
`compose`'s local **is** the unit's `@components`, and BUILD's parameter loses to
it: the array object BUILD receives is literally the one the previous, unrelated
client-side `compose` built (verified by `.WHICH` — same address).

Minimal proof against the real dist, no synthetic code needed:

```raku
my @components = <SENTINEL>;
my $service = Cro::HTTP::Server.new(:host('localhost'), :port(31319), :$application);
$service.start;
await Cro::HTTP::Client.get("http://localhost:31319/");
say @components.map({ .^name }).join(",");
# Cro::HTTP::RequestSerializer,Cro::TCP::Connector,Cro::HTTP::ResponseParser
```

Renaming *either* side in a shadow copy of `Cro.rakumod` makes the whole suite's
multi-server files construct correctly, which is how the collision was confirmed.

Affected files in `tmp/cro-work/C_RO_CRO_HTTP_*/t/`: `http-middleware`,
`http-auth-basic`, `http-auth-basic-with-session`, `http-session-inmemory`,
`http-session-persistent`, `router-auth`, and the second half of `http-router` —
all of them start more than one server.

Pin when fixed: `tmp/nb16.p6`'s matrix as `t/thread-callee-array-does-not-clobber-caller.t`,
alongside the existing scalar pin `t/thread-callee-param-does-not-clobber-caller.t`.

## Secondary anomaly seen in the same repro, not yet isolated

Looping `for 1..3 -> $i { say "round $i"; ...Cro request...; say "round $i status" }`
against the Cro server printed `round 2 status` in **every** iteration, including
the first — while the plain `say "round $i"` before the request was correct each
time. A synthetic `for` + `await start { }` does not reproduce it (`tmp/nb17.p6`),
so it needs whatever the Cro request path does. It may well be the scalar-side
sibling of the same shared-lane aliasing; worth re-checking once the array half is
fixed rather than chasing separately.
