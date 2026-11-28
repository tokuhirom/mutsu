# ADR-19368: Run Cranelift's IR verifier in debug builds and stress runs, not in release builds, and make a verifier failure loud

- **Status**: Accepted (maintainer decision 2026-11-28; proposed 2026-11-28)
- **Issue**: [#19368](https://github.com/tokuhirom/mutsu/issues/19368); campaign A carrier
  [#19288](https://github.com/tokuhirom/mutsu/issues/19288)
- **Related**:
  - [ADR-0004](0004-jit-strategy.md): the Cranelift JIT.
  - [ADR-19288](19288-native-int-num-kernels.md): native kernels, which share the JIT engine.
  - [ADR-0116](0116-trir-native-lowering-measured-before-building.md) §3.2: measured the verifier
    at 13.9M of a lowering's 128M compile instructions, and named "verifier off in release builds"
    as one condition for revisiting native TRIR lowering.
  - [docs/security.md](../security.md): the trust boundaries this change has to respect.

## 1. Context

### 1.1 What the setting is for

mutsu creates one process-wide Cranelift JIT engine (`src/vm/vm_jit_compile.rs::engine_in`). The
bytecode JIT, the sub kernels and the loop kernels (ADR-19288) all compile into it. The engine
sets `opt_level=speed` and leaves every other shared flag at its default. That includes
`enable_verifier`, whose default is `true`.

Cranelift's own documentation of the flag (`cranelift-codegen` 0.138,
`settings::Flags::enable_verifier`; the definition is in `cranelift-codegen-meta`
`src/shared/settings.rs`):

> Run the Cranelift IR verifier at strategic times during compilation.
>
> This makes compilation slower but catches many bugs. The verifier is always enabled by
> default, which is useful during development.

So Cranelift positions the verifier as a development aid that is on by default for the developer's
convenience. It is not presented as a safety feature for production code generators.

Wasmtime is the main production embedder of Cranelift. It exposes the flag as
`Config::cranelift_debug_verifier` and documents its default as `false`, adding that the check is
mostly useful to Wasmtime's own developers. Production JITs built on Cranelift ship with the
verifier off and turn it on in their test and fuzzing setups.

### 1.2 What it costs mutsu

The cost was measured with callgrind on the profiling build (release optimizations plus line
tables). The verifier entry is `cranelift_codegen::verifier::verify_context`, inclusive.

| workload | total Ir | verifier Ir | share |
|---|---:|---:|---:|
| `benchmarks/poly-call.raku` | 239.2M | 16.5M | 6.9% |
| method-call micro (`$p.m0`, 11k iterations) | 98.9M | 2.4M | 2.4% |
| concat micro (11k iterations) | 120.3M | 3.8M | 3.2% |
| `benchmarks/bench-class.raku` | 588.6M | 13.7M | 2.3% |
| `benchmarks/bench-ctor.raku` | 1186.6M | 5.7M | 0.5% |

The verifier is about 42% of Cranelift's whole compile time (16.5M of 39.0M on poly-call). Its
share is largest on the short programs where compile time already dominates. Those are exactly
the rows of campaign A whose wall time includes interpreter start and JIT warm-up.

### 1.3 What it does for mutsu today: nothing visible

Every compile path ends in `module.define_function(id, &mut ctx).ok()?`: the bytecode JIT's
`build`, the sub-kernel and loop-kernel `build`s. With the verifier on, an invalid function makes
`define_function` return an error. `.ok()?` turns that into `None`, which every caller reads as
"this region declines". The code then runs on the interpreter, and nothing is logged, counted or
asserted.

So in every build the verifier is a cost with no observer. It cannot be what catches a mutsu
codegen bug today, because when it catches one nobody hears about it. The only protection it
gives is that invalid IR is never executed; the bug itself stays hidden.

## 2. Decision

### D1. The verifier follows the build profile

`enable_verifier` is set from `cfg!(debug_assertions)` when the engine is created:

- **on** in debug builds: `cargo build`, `cargo test`, and the `scripts/dev gate` quick profile,
  whose `prove` runs the debug binary;
- **off** in release, profiling and benchmark builds, and so in the binaries the release workflow
  ships.

### D2. `MUTSU_JIT_VERIFY` overrides it at run time

`MUTSU_JIT_VERIFY=1` forces the verifier on and `MUTSU_JIT_VERIFY=0` forces it off. The variable
is read once, when the engine is created, like the other `MUTSU_JIT*` knobs. A release binary can
then verify its JIT output while someone investigates a suspected miscompile, without a rebuild.
The binary the stress jobs share (D4) can verify too.

### D3. A verifier failure is loud

With the verifier on, a `define_function` error that is a verifier error is reported, not
swallowed:

- in a debug build or under `MUTSU_JIT_VERIFY=1` it panics. The message names the compiler (the
  bytecode JIT, a sub kernel or a loop kernel) and the code unit, and carries Cranelift's
  verifier report with the function's IR. The test or stress run that hit it fails at that
  point;
- every other compile error keeps today's behaviour: the region declines and runs on the
  interpreter.

This turns the verifier from an invisible cost into the bug detector Cranelift describes. The
panic happens before any native code exists. It is a panic, not undefined behaviour, and it
happens only in builds and runs that asked for verification.

### D4. The stress runs verify everything they compile

The nightly `jit-stress` job (`.github/workflows/stress.yml`, `MUTSU_JIT_THRESHOLD=2`) also sets
`MUTSU_JIT_VERIFY=1`. The threshold of 2 compiles almost every chunk the TAP suite runs, so every
night the release binary's JIT output is verified across the whole suite. Debug-build
`cargo test` and the gate do the same for every change before it lands.

## 3. Security

AGENTS.md forbids weakening a security protection without the maintainer's prior approval. The
maintainer gave the direction for this change on 2026-11-28 ("off in release and benchmark
builds"). The analysis behind it:

- **What the verifier checks.** It checks the well-formedness of IR that mutsu's own lowering
  code generates: SSA form, types, block structure. It does not look at program data. The input
  to the lowering is mutsu bytecode, compiled from a script the user chose to run. Running a script
  is the trusted boundary of docs/security.md, and the lowering admits only closed op lists
  (ADR-0004, ADR-19288 D1). Untrusted *data* (file contents, network input, string values) never
  decides the shape of the IR; it only flows through it as runtime values that the emitted guards
  check.
- **What changes when it is off.** Valid IR compiles exactly as before; the verifier never alters
  code. Invalid IR, which would be a mutsu codegen bug, is no longer stopped at compile time in a
  release build. Cranelift may then panic during lowering or emit wrong code. That is the one real
  risk, and D3 and D4 address it. The bug class is the one Cranelift's own users manage the same
  way: verify in development, tests and fuzzing, not in production.
- **What changes in practice.** Today such a bug is silently routed to the interpreter in every
  build and never fixed. After this change, debug builds and the nightly stress run panic on it,
  so it is found and fixed. Release builds stop paying for a check whose result was thrown away.
- Nothing else moves. The JIT's other invariants are unchanged: code memory owned and finalized by
  `cranelift-jit`, the `ENGINE` mutex, the `// SAFETY:` comments at every native entry, and a
  kernel frame that holds no `Value` (ADR-19288 D4).

## 4. Consequences

- Release-build JIT compiles get about 40% cheaper. Short programs and campaign A's
  compile-bound rows gain the shares in §1.2: about 7% of poly-call and 2-3% of the
  method-call and bench-class rows.
- ADR-0116's first revisit condition for native TRIR lowering ("compilation is cheaper ... via
  Cranelift's verifier off in release builds") is met.
- A codegen bug that the verifier catches stops being silent. Expect the first runs with D3 to
  surface existing ones; each becomes an issue or a fix, not a reason to turn the check back off.
- Debug builds compile JIT code a little more slowly than now, because a failure is now reported.
  That is noise next to an unoptimized debug build.

## 5. Alternatives rejected

- **Keep it on everywhere (status quo).** It pays 2-7% on the rows above for a check whose
  result is discarded (§1.3).
- **Off everywhere.** It loses the only systematic check of the lowering code, which keeps
  growing (loop kernels, element helpers). Cranelift's documentation recommends exactly the
  opposite for development.
- **A Cargo feature instead of `cfg!(debug_assertions)` plus an environment variable.** A feature
  needs a separate build to turn the check on, and the stress jobs deliberately share one
  binary configured by runtime variables (`MUTSU_GC`, `MUTSU_JIT`). The environment variable fits
  that model, and `debug_assertions` already tracks "development build" without a new switch.
- **Keep the verifier on but stop swallowing its error.** That fixes §1.3 but keeps the release
  cost, which nothing justifies once debug builds and the stress run verify.

## 6. Open questions

- Whether the panic in D3 should instead record the failure (a counter in `MUTSU_VM_STATS` plus
  the IR on stderr) and decline, so one bad region does not end a long stress run. To be decided
  when the first real verifier failure is seen; the panic is the stricter starting point.
