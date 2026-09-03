# `Interpreter::new()` costs ~9 ms and retains ~7 KiB per construction

Measured 2026-09-03 while building ADR-0065 S2, which briefly constructed one
`Interpreter` per document analysis and made both properties visible:

| Metric (debug build, 4000 constructions, construct-and-drop) | Value |
| --- | --- |
| Wall clock | **9.17 ms per `Interpreter::new()`** |
| Resident memory | **+7.2 KiB per construction**, linear (28.9 MB over 4000) |
| Interned symbols | +0 |

The memory is *retained*, not merely slow to return: growth stayed linear from
1000 to 4000 constructions (7.46 → 7.22 KiB/call), so the allocator is not
recycling it. `MUTSU_GC=on` changes nothing (9.26 ms, 7.31 KiB/call), so this is
not a GC cycle waiting for a collector that never runs in that loop.

For scale: on the same build, parsing a 1140-byte document costs about 5 ms and
retains 0.5 KiB. **Constructing an interpreter is roughly twice as expensive as
parsing a whole document, and leaks fifteen times as much.**

## Why it matters beyond the language server

S2 dodged it — the analysis frontend now reaches the same verdict through
`check_undeclared_routines_without_interpreter`, with no `Interpreter` at all —
so nothing is currently blocked. But the cost is paid elsewhere:

- `Interpreter::new_regex_scratch` (`runtime/runtime_init.rs`) constructs one
  per regex scratch use. Whether that is on a hot path is unmeasured.
- Any embedder driving mutsu per request pays it per request.
- A future language-server slice that genuinely needs interpreter state (S4's
  `workspaceSymbol` across many files, say) will hit it again.

## Where to look

`Interpreter::new` (`src/runtime/runtime_init.rs:44`) reads as pure data setup:
the dynamic-variable environment (including a full `std::env::vars()` sweep into
`%*ENV`, skipped only for `is_building_scratch`), then the core class registry
built as a long series of `ClassDef` literals. Nothing in the part that was read
during this investigation obviously *retains*, so the retention is the part
worth chasing first — a `Box::leak`, a push into a process-global registry, or a
`Gc` allocation that outlives the drop.

The `%*ENV` sweep is the obvious wall-clock suspect and already has a bypass
(`BUILDING_SCRATCH`); whether it dominates the 9 ms is unmeasured.

## Repro

Add to `tests/long_lived_parse.rs` (it already has `rss_kib`, `iterations` and
the serialization guard) and run with `MUTSU_S0_ITERATIONS=4000`:

```rust
#[test]
fn interpreter_new_cost() {
    let _guard = exclusive();
    let n = iterations();
    for _ in 0..3 { let _ = mutsu::Interpreter::new(); }
    let r0 = rss_kib();
    let t0 = Instant::now();
    for _ in 0..n {
        std::hint::black_box(mutsu::Interpreter::new());
    }
    println!("{:?}/call, rss {:+} KiB", t0.elapsed() / n as u32,
        rss_kib().unwrap_or(0) as isize - r0.unwrap_or(0) as isize);
}
```

Per `todo/README.md` these are debug-build numbers; re-measure in release before
designing a fix, and take any figure that ends up in a document from the bench
CI rather than from a local run.
