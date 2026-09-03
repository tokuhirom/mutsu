# The call path stopped re-interning `"_"` and `"Any"`, and stopped cloning a bound argument twice

A `perf` profile of `benchmarks/bench-fib.raku` still showed `Symbol::intern`
and `std::thread::local::LocalKey::with` in the top twenty after the ADR-0037
frame-push work. Neither belongs on a call path that resolves no user names:
`Symbol::intern` hashes the whole string and takes a thread-local borrow, so
re-interning a *fixed* name once per call is pure overhead.

Three fixed names were being re-interned on every compiled call:

- `unmark_readonly("_")` — every routine entry clears any readonly mark the
  caller's topic left on `$_`, and the `&str` overload interns `"_"` to get
  there (`vm_call_light.rs`, `vm_call_light_typed.rs`).
- `Symbol::intern("Any")` — the value a routine's fresh topic is seeded with
  (`vm_call_light.rs`, `vm_call_light_typed.rs`, `vm_call_fast.rs`).
- `env.insert("_".to_string(), …)` / `env.get("_")` / `env.remove("_")` —
  `Env`'s `&str`-keyed methods are all `…_sym(Symbol::intern(key))`, so each
  one interned `"_"` again, and the `insert` also allocated a `String` per
  call.

`crate::symbol::wk` now holds these as process-wide pre-interned symbols
(`wk::topic()`, `wk::any()`, `wk::file()`), resolved once behind a `OnceLock`
and a plain load afterwards. Interned ids are global and append-only, so
caching one is valid for the life of the process and across threads.
`Env::file_key` — added the same day for
`news/2026-09/env-carries-its-source-file-symbol.md` — moved into the same
module rather than keeping a second private copy.

## The double clone in the parameter bind loop

The positional-light bind loop also cloned each bound argument twice:

```rust
self.locals[*slot] = val.clone();
let needs_env = …;
if needs_env {
    self.env_mut().insert(param_name.clone(), val);
}
```

When `needs_env` is false — the common case for a routine whose parameters are
read only through their local slots — `val` was cloned into the slot and then
dropped, where a move would do. When it is true, the env mirror was keyed by a
freshly allocated `String` that `Env::insert` then interned, even though
`CompiledFunction::param_name_syms` already holds the parameter name
pre-interned for exactly this reason.

Now the `needs_env` branch writes the slot and mirrors through `insert_sym`
with the pre-interned name (calling `note_env_key` on a borrow so the key-shape
bookkeeping `Env::insert` performs is preserved), and the common branch moves
the value straight into the slot.

## Result

Local interleaved release A/B (median of 15 alternating runs, idle box):
`bench-tak` −8.0%, `bench-fib` −5.2%, `fib` −4.2%, `bench-ctor` −2.0%;
`hash-access`, `method-call` and `word-count` neutral (each re-measured in both
orderings — a single ordering showed a 3.9% swing that reversed). `bench-tak`
gains most because it takes three parameters, and the bind-loop saving is per
parameter per call.

`make test` (3625 files) and a targeted roast sweep of the 440 whitelisted
`S02` / `S03` / `S04` / `S06` / `S32-exceptions` files pass. The bench CI is the
authority for figures that end up in documents.
