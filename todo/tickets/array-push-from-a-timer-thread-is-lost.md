# A tap callback's `@array.push` from a timer/scheduler thread is lost

```raku
my @res;
my $done;
Supply.interval(0.1).head(3).tap({ @res.push($_) }, :done({ $done = True }));
for ^40 { last if $done; sleep .1 }
say "done=$done res=@res[]";
```

```
$ raku                     $ mutsu
done=True res=0 1 2        done=True res=
```

The same inside a routine returns `[]` instead of `[0, 1, 2]`.

## What the repro isolates

The `:done` callback's write to the **scalar** `$done` crosses the timer thread
correctly; only the **array** pushes are lost. So the failure is in the
`@`-aggregate lane of the cross-thread shared store — the `__mutsu_atomic_arr::`
CAS copies that `set_shared_var_sym` keeps out of the way of stale local
snapshots — not in the scalar lane. It is adjacent to `354cd623f` ("an array
alias survives a thread having run") and to the hyper-block mutator fix pinned
by `t/hyper-array-mutators.t`, and is *not* the parameter-aliasing bug fixed in
`news/2026-08/a-callee-parameter-is-not-a-shared-variable.md` (re-measured with
that in: unchanged).

Worth checking whether the emitting side matters: `Supply.interval` cues on the
scheduler's timer thread, whereas `Supply.from-list(...).tap` emits on the
tapping thread and collects correctly.

## What it blocks

Five of the six regressions in `todo/tickets/retire-native-test-tap.md`
(`S17-supply/classify.t`, `categorize.t`, `interval.t`, `merge.t`, `reduce.t`),
which is on the critical path for
`todo/tickets/vendor-real-test-module.md` step 3. The real `Test::Tap`'s
`tap-ok` collects with exactly this shape:

```raku
my @res;
$s.tap({ emit() if &emit; @res.push($_) }, :done({ … }));
…
is-deeply @res, $expected, $desc;
```

so every one of them reports `# expected: [0, 1, 2, 3, 4]` / `# got: []`. All
five pass under `raku`.
