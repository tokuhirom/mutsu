# A matching `when` no longer aborts the rest of a `Junction.THREAD` loop

`Junction.THREAD` calls its block once per eigenstate. A matching `when` inside
that block leaves *the block* with a `succeed` control signal — but `.THREAD`'s
loop propagated it as an ordinary error, so the remaining eigenstates were
skipped and the enclosing routine unwound with the `when` body's value.

```raku
sub collect(Junction $j) {
    my @seen;
    $j.THREAD: {
        when Junction { @seen.push: 'J' }
        @seen.push: $_;
    }
    @seen;
}
say collect(any(all(1, 2), 3));   # raku: [J 3]   mutsu, before: never returned normally
```

Every other loop construct already absorbs `succeed` and continues with the next
iteration (see the `for` body in `vm_for_loop_body.rs`); `.THREAD` now does the
same. `for` over the same values was correct throughout, which is why this
stayed hidden.

It surfaced through roast's `Test::Util`, whose `is-deeply-junction` guts a
junction with exactly this shape — recursively, so a nested `any(all(1,2), 3)`
came back as just the `all` part. Pinned by `t/junction-thread-when.t`, which
passes under `raku` as well.
