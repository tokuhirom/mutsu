# Hash element store and container reads take the constant pool's Symbol

`hash-access` and `bench-hash` had been drifting upward at roughly 2%/day with
no single culprit commit ([#7559](https://github.com/tokuhirom/mutsu/issues/7559)).
A callgrind bisect had already established the shape of the problem — diffuse,
no frame dominating the delta — and named the largest remaining item precisely:
`Symbol::intern` was still called about 155,000 times for a benchmark with
20,000 loop iterations, almost all of it re-interning *constant* names on hot
paths.

The env is `Symbol`-keyed, so every by-name helper (`Env::get`,
`Env::get_mut`, `Env::contains_key`, `Interpreter::is_readonly`) begins by
interning its `&str` argument: a thread-local `RefCell` borrow plus a string
hash. But the names those helpers were asked about are opcode operands —
constant-pool indices whose string the compiler fixed long before the loop ran.
`CompiledCode::const_sym` already memoizes the `Symbol` for a constant-pool
entry, and `GetGlobal`'s scalar shortcut already used it; the element-store and
container-read paths did not.

This change threads that memoized `Symbol` through them.

- **`try_fast_hash_element_assign`** resolved `var_name` once and now passes the
  symbol to all four of its probes (`env().get`, `env_mut().get_mut`, the local
  slot re-read, and `is_readonly`), which each re-interned the same name.
- **`exec_index_assign_expr_named_op`** and its two inner layers do the same for
  the `Range`-receiver guard, the unit-lexical cell seed/restore, the `Seq`/
  `Proxy` destination lookup, and the six container-metadata probes that bracket
  the assignment.
- **`get_env_with_main_alias`** gained a symbol-taking twin,
  `get_env_with_main_alias_sym`, used by `GetGlobal`, `GetArrayVar` and
  `GetHashVar`. This is the single largest item: it is the *only* read path an
  `@`/`%` name has, because `GetGlobal`'s scalar shortcut excludes those sigils
  by construction, so a container-heavy program paid one intern per element
  access.
- **`reify_lazy_array_slot`** — a probe that runs on every element store and
  delete for the sake of the rare lazy `@`-array — takes the symbol too.

Three allocations went with them. The element-store path built a `String` copy
of the variable name on every store (three times over, across its layers) purely
to hand a `&str` to helpers; the name is borrowed from the constant pool
instead, which outlives the op and is a distinct borrow from `&mut self`. The
hash fast path stringified its key twice and then cloned it a third time; it now
stringifies once and moves the `String` into the insert, rebuilding it only in
the `%*ENV` branch that no ordinary hash takes. And `unit_lexical_container_cell`
— consulted on every element store and delete — now opens with the same
`unit_lexicals.is_empty()` gate `unit_lexical_slot` already had, so a program
whose mainline subs capture nothing skips two SipHash string lookups per store
rather than missing them one at a time.

Instructions retired (`MUTSU_JIT=off`, callgrind, one release build per point —
Ir is stable across runs and unaffected by runner speed, which is why the ticket
uses it rather than wall clock):

| benchmark | before | after | delta |
| --- | --- | --- | --- |
| `hash-access` | 240,872,402 | 220,592,761 | -8.4% |
| `bench-hash` | 294,077,225 | 272,657,254 | -7.3% |

That recovers roughly four days of the creep the ticket measured. The other
benchmarks are unmoved (`array-ops` -0.8%; `bench-array`, `bench-string`,
`bench-mandelbrot`, `bench-class`, `method-call`, `word-count` all within
±0.2%), which is the expected shape: the change removes work from paths a
container-heavy program runs per element and leaves scalar-heavy code alone.

`t/hash-element-store-symbol-keyed.t` pins the behaviour the symbol-keyed probes
must keep agreeing on — the plain fast path and each of its bail-outs (readonly
binds, `:=`-bound elements, typed hashes, `is default(...)`), the unit-lexical
cell in its open state, the lazy-array reify and its delete twin, and `%*ENV`.

What the ticket lists as still open is untouched and stays open: the remaining
`Symbol::intern` callers that build a *fresh* name (`format!`-ed metadata keys),
and `HashData`'s use of std's SipHash, which is 6% of `hash-access` but trades
away HashDoS resistance on user-controlled keys and so needs an explicit
decision, not a drive-by change.
