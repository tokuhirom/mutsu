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

Two costs are worth separating before optimizing:

1. **Per-`start` overhead** — task spawn, env capture, `await`. 31k spawns is a
   lot even for a fast runtime; raku pays it too, so this is a ratio question,
   not a structural one.
2. **The `shared_vars_active` latch.** The first `start` in a process turns on
   the name-keyed shared-variable lane *permanently* (`runtime_thread.rs`), and
   from then on ordinary lexical reads/writes take different, slower paths in
   the VM. So the per-round interpreter work inside each `start` may itself be
   running in the degraded mode. Measuring a plain (non-`start`) loop before and
   after a single `start` would separate the two — if the post-`start` loop is
   itself much slower, this is the bigger lever, and it would speed up *every*
   threaded program, not just this one.

That latch has already produced one silent correctness bug — the native-array
push path skipped element typing while it was on
(`news/2026-08/native-array-push-after-a-start.md`) — so understanding what else
changes behind it has value beyond this benchmark.

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
