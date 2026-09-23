# A closure captures the free variables of the inner subs it calls

A named sub declared inside a routine reads its free variables from the env
that is live when it is *called*, not from its declaring frame (ADR-0024 gives
mainline and bare-block subs lexical resolution, but leaves routine-nested
subs on the old dynamic path). That is harmless while the routine is still
running, but a closure that escapes the routine only captured its *own* free
variables, so once the routine had returned the inner sub found none of its
own:

```raku
sub escaping($p) {
    my sub twice() { $p * 2 }
    -> $x { twice() + $x }
}
say escaping(7)(1);   # raku: 15, mutsu was: 1
```

The compiler now records the free variables (reads and writes) of every
named sub declared inside a routine body in a lexically inherited table
(`Compiler::lexical_sub_free_vars`). A call site that names one of those subs
folds the callee's free variables into its own `nested_routine_free_reads`,
the same channel a directly nested routine already uses. The closure then
captures `$p` itself, so each activation of the routine keeps its own binding.
A variable the inner sub mutates stays a shared cell. Because a sibling
inner sub compiles against the same table, the capture is also transitive:
`-> { u() }` where `u` calls `t` carries `t`'s variables too.

Two cases still use the old dynamic lookup, and ADR-0024's known limitations
now list them. One is a `&t` code object that itself escapes the routine.
Fixing it needs a separate environment for each activation of the inner sub.
The other is a closure that declares its own local with the name of the
inner sub's free variable. That was already broken before this change.

Pinned by `t/routines/closure/closure-calls-routine-nested-sub.t`
(mutsu#9106).
