# `Digest::RIPEMD` is 11x raku — a `start` per compression block

`rmd160` is correct on every RFC vector, but the bundled `Digest` battery's
`t/ripemd.t` is the one upstream file that cannot be whitelisted: it takes
~513s against raku's ~46s, over the batteries gate's 120s per-file budget
(`docs/batteries/digest.md`). The cost is entirely in the last vector,
`'a' x 1_000_000`.

## Where it goes

`modules/Digest/lib/Digest/RIPEMD.rakumod` runs the two halves of each
compression round concurrently:

```raku
blob32.new: [Z+] map {$_[[^5].rotate(++$)]}, $h, |await
  map -> [&f, $r, @K, $s] {
    start { reduce -> $A, $j { … }, $h, |^80 }
  }, …
```

A 1 MB message is 15625 blocks, so the run spawns ~31k `start` tasks, each
doing an 80-round reduce. That is ~33ms per block; raku manages ~3ms.

Two candidate costs were measured (release build, 2026-08-05):

1. **Per-`start` overhead is the confirmed lever — ~17x raku.**

   ```raku
   for ^2000 { await map -> $k { start { $k * 2 } }, 1, 2 }
   # mutsu 5.53s   raku 0.332s
   ```

   That is the exact shape `rmd160` runs per block. At 15625 blocks it is ~43s
   of pure spawn/await overhead in mutsu against ~2.6s in raku — a large slice
   of the gap, though not all of it, so there is per-round interpreter cost on
   top.

2. **The `shared_vars_active` latch is NOT the problem.** The first `start` in a
   process turns on the name-keyed shared-variable lane permanently
   (`runtime_thread.rs`), which looked like it might degrade every later
   lexical access. Measured: a 200k-iteration scalar+array-push loop runs
   0.576s before a `start` and 0.402s after it. No degradation — do not spend
   time here.

   (That latch is still worth understanding: it produced one silent correctness
   bug, the native-array push path skipping element typing while it was on —
   `news/2026-08/native-array-push-after-a-start.md`.)

## Repro

```sh
cargo build --release
D=tmp/libdigest-clone   # or any checkout of grondilu/libdigest-raku
timeout 900 ./target/release/mutsu -I modules/Digest/lib $D/t/ripemd.t
# 9/9 correct, ~513s     (raku -I lib t/ripemd.t: ~46s)
```

A smaller handle on the same path:

```sh
./target/release/mutsu -e 'use Digest::RIPEMD; rmd160("a" x 100_000)'
```

## Not a blocker

Every digest in the dist is correct, and the other three upstream files are
whitelisted and fast (2.2s / 1.5s / 5.9s). This ticket is about closing the
throughput gap so `ripemd.t` can join them.
