# A method-call site replays its last plan or accessor read up front

A call on an ordinary object that already had a method-call plan (ADR-15737 D1) or a generated-accessor memo (ADR-0121 D3) still walked a dozen layers on every call before reaching it:

- the site lanes for built-in methods and iterators;
- the lazy-named-argument and `.VAR` checks;
- the instance front and the accessor-lane probe;
- the plain-method lane key, which interns one type key per argument;
- two hash probes, into the lane set and the plan table.

On `$p1.distance-to($p2)` that was about 3k instructions of a 16k call.

Each call site now remembers, in a per-interpreter table keyed by the chunk and the instruction, which of the two memos answered its last call (`src/vm/vm_method_site_ic.rs`). The next call checks a few things up front and then replays the memo directly:

- the receiver's class;
- the argument type keys;
- the registry method and write generations;
- the process-wide gates the skipped layers test: an accessor-ref request, a user `^find_method`, a wrap chain, `use fatal`.

An entry is written only by the memo that just answered the call, so it holds exactly what that memo is keyed on. Only plans that run in the lean or slot frame are replayed. A body outside them may rebind its caller's receiver through `CALLER::`, and the site's writeback check stays on the full path for it.

Callgrind slope per loop iteration, profiling build, on top of the slot-frame op widening:

| input | before | after |
| --- | ---: | ---: |
| `$sum += $p1.distance-to($p2)` (method-call's method) | 17.5k Ir | 14.7k Ir (-16%) |
| bench-class's `full-info` + `bark` iteration | 43.3k Ir | 36.4k Ir (-16%) |
