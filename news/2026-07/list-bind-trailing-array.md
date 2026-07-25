# A trailing `@x` in a `:=` binding is not slurpy

In a signature **binding** (`my (...) := ...`) a plain `@x` binds one positional
argument; only an explicit `*@rest` is slurpy. mutsu treated a *trailing* array
target as implicitly slurpy, so the last one came back wrapped in an extra layer:

```raku
my @x = 1, 2;
my @y = 5;
my (@a, @b) := (@x, @y);
say @b.raku;    # was [[5],]  --  raku: [5]
```

Every array target except the last bound correctly, which made this easy to miss.
The comment above the code already spelled out the right rule and called the
slurpy branch a "historical heuristic"; the heuristic is now gone in binding mode.
List **assignment** (`=`) keeps its own, different greedy semantics — there the
*first* `@`/`%` target slurps and every later target gets an empty container —
which is unchanged.

Found while triaging `TODO_dist` ticket T-037 (Test::Scheduler), whose
`method !run-due` opens with

```raku
my (@now, @future) := $!lock.protect: {
    my (:@now, :@future) := @!future.classify: { ... }
    ...
};
```

The doubly-wrapped `@future` made the scheduler believe work was still pending, so
`advance-by` silently ran nothing.

Pin: `t/list-bind-trailing-array.t` (11 assertions, passes under both mutsu and
raku).
