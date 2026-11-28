# An inner loop that reads an outer loop's parameter runs as a kernel

`carried_slots` required every slot a loop kernel carries in to be declared
`Untyped` by the chunk, because a kernel's store skips the constraint check a
typed variable's store makes. An enclosing `for ... -> $t` parameter has no
declaration, so its constraint reads `Unrecorded`. Reading `$t` in the inner
loop therefore declined the whole kernel:

```raku
for ^4 -> $t {
    for ^7000 -> $i { @slots[$t * 16 + ($i +& 15)] = $t * 1000 + $i }
}
```

The requirement now applies only to the slots the region writes (`SetLocal`,
`++`/`--`, a meta-assign). A carried slot the region only reads keeps its
machine word through the run, and the exit writeback skips an unchanged word,
so no store ever reaches it and no constraint can be bypassed. Typed and
native variables that the loop writes still decline, as before.

callgrind, profiling build, JIT on, `BENCH_DET=1`:

| file | before | after |
|---|---:|---:|
| `benchmarks/bench-threads-serial.raku` | 160,978,824 Ir | 84,073,008 Ir (-48%) |
