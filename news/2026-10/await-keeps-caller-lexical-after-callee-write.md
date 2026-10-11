# `await` no longer overwrites a caller's lexical with a callee's same-named one

Cache::Async 0.3.4's `t/08-refreshing.rakutest` passed 1 of 10 subtests
([#11545](https://github.com/tokuhirom/mutsu/issues/11545)):

```raku
my $lock = Lock.new;
sub get() {
    my $now = Nil;
    $lock.protect({ $now = 7; start { } });
    Promise.kept(1)
}
for 1..3 {
    my $now = "main$_";
    await get();
    say $now;          # raku: main1 main2 main3; mutsu: main1 7 7
}
```

Once a thread exists, a write to a captured scalar from a closure is also
published to the cross-thread shared store, which is keyed by bare name. The
callee's `$now = 7` went in under `$now`. The caller's `await` then synced every
written entry back into the awaiting thread's env, so the callee's local landed
in the caller's unrelated `$now`.

#12227 had already stopped `await` from pulling entries that were only seeded
at spawn. Now each written entry also records which lineage (thread interpreter)
wrote it, and `await` skips the entries its own lineage wrote. Those writes are
already in its own env. Writes from other threads still arrive as before.
Recording the lineage, not the OS thread id, matters because pool threads run
many interpreters on one OS thread. Cache::Async now passes all 10 of its test
files, as under rakudo.

The store is still keyed by bare name. Re-keying it by binding identity is
#12204.

Pin: `t/concurrency/promise/await-keeps-caller-lexical-after-callee-write.t`.
