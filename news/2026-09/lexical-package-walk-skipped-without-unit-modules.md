# A routine call stops computing a lexical package it almost never has

[#7743](https://github.com/tokuhirom/mutsu/issues/7692) gave `RoutineFrame` a
`lexical_package` field, so that a `unit module`'s routines resolve the module's
own imported aliases lexically rather than inheriting them from whatever scope
loaded the file. It is computed on **every routine call**, and the computation
is not cheap:

```rust
let unit = def_file
    .map(|file| {
        let source = file.resolve();          // heap-allocates a String copy of the path
        self.unit_of_source(Some(&source))    // …then re-interns that copy
    })
    .unwrap_or(self.current_unit);
loop {
    if let Some(package) = self.unit_module_packages.get(&unit) {
        return Some(*package);
    }
    unit = crate::runtime::eval_unit_parent(unit)?;   // read of a process-global RwLock
}
```

Per call: a `String` allocation, a re-intern of the declaring path, and a read of
the process-global `EVAL_UNIT_PARENTS` lock. A program that declares no
`unit module` / `unit class` pays all of it and can never get an answer back —
the only `return Some` is a hit in `unit_module_packages`, so an empty table
decides the result on its own.

The `file.resolve()` → `unit_of_source(&str)` → `Symbol::intern` round trip is
also the exact pattern ADR-0037 removed from this call path once already
(`news/2026-09/adr0037-routine-frame-push-intern-cost.md`: interning the frame's
names per call cost ~26% of `bench-fib`).

## The change

Two edits to `lexical_package_for_frame`, both equivalent by construction:

- **Return `None` immediately when `unit_module_packages` is empty.** The guard
  tests the condition that already decides the result, so no program's answer can
  change — it only stops deriving that answer the expensive way.
- **Pass the `Symbol` through.** `def_file` is already interned; hand it to
  `unit_of_source_sym` instead of resolving it back into a `String` for
  `unit_of_source` to re-intern. `Symbol::intern` is idempotent, so both
  spellings name the same unit. This is what remains for programs that *do*
  declare a unit module.

## Measured

`benchmarks/bench-fib.raku` under callgrind, `--profile profiling`. Instruction
counts are deterministic, so these are work rather than noise — and the
JIT-emitted region is byte-identical (178,247,076 Ir) across every build below,
which rules out a JIT bailout as the cause.

| | bench-fib Ir | |
| --- | ---: | ---: |
| `2a79d02c` (main before this change) | 1,378,988,263 | |
| **with this change** | **1,303,982,460** | **−5.44%** |
| `398009f9` (#7742, before the regression) | 1,307,166,471 | this change lands 0.24% below it |

Per function, with **no function increasing**:

| function | before | after |
| --- | ---: | ---: |
| `eval_unit_parent` | 21,610,175 | **0** |
| `push_routine_with_location` | 43,856,545 | 36,864,866 |
| `call_compiled_function_positional_light_at` | 536,432,606 | 490,036,910 |

## How it was found

The audit of [#7579](https://github.com/tokuhirom/mutsu/issues/7579) noticed
`bench-fib` had grown 7.0% in retired instructions since early September and
bisected it across the merge range, one `--profile profiling` build per
candidate:

| commit | PR | bench-fib Ir | vs previous |
| --- | --- | ---: | ---: |
| `4c116213` | #7731 | 1,288,756,087 | — |
| `398009f9` | #7742 | 1,307,166,471 | +1.43% |
| **`d7a69060`** | **#7743** | **1,370,741,903** | **+4.86%** |
| `4ab7b9ae` | #7744 | 1,370,732,315 | −0.00% |
| `4468e30e` | #7761 | 1,370,731,612 | −0.00% |
| `2a79d02c` | #7762 | 1,378,988,263 | +0.60% |

Two method notes worth carrying forward, both learned the hard way here:

- **Retired instructions are insensitive to code *placement*, not to
  *inlining*.** #7579's method notes recommend instruction counts to escape the
  ~5% layout lottery, which is right, but an inlining decision that flips when
  unrelated code grows does change the instruction count. Three of the four
  functions that appeared in the diff were 0 → N purely because they stopped
  being inlined; only the caller's own delta (−1.9 M against +63.6 M of new
  callee cost) established that the work was real.
- **A plausible attribution is not an attribution.** The first guess here was
  #7762, because it demonstrably put a `Symbol::resolve` on the light-call path
  and its news entry named the function. Building its parent commit showed #7762
  contributed +0.60%, not the +7%; the culprit was three merges earlier. The
  bisect cost four builds and was the only thing that settled it.

`t/module-import-alias-scope.t` pins the import scoping #7743 fixed, and stays
green.
