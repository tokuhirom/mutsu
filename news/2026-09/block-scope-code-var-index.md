# A block scope no longer scans the whole env to save its routine bindings

`eval_block_value_inner` — the carrier that runs a `where` clause, a role type
argument, an `EVAL`'d unit and, above all, a regex `<?{ … }>` assertion body —
has to save the `&`-routine bindings visible in the scope it runs in and put
them back on exit, so a block-local `sub foo { }` or `my &foo = …` cannot leak
into the caller. It did that with two full walks of the env:

```rust
let saved_code_env: HashMap<Symbol, Value> = self.env.iter()
    .filter(|(k, _)| k.starts_with("&") || k.starts_with("__mutsu_callable_id::"))
    .map(|(k, v)| (*k, v.clone())).collect();
// ... body ...
self.env.retain(|k, _| !(k.starts_with("&") || k.starts_with("__mutsu_callable_id::")));
for (k, v) in saved_code_env { self.env.insert_sym(k, v); }
```

A carrier block is not a rare thing to run. A `<?{ … }>` assertion is evaluated
once per *cursor position*, and a grammar `token` body runs dozens of times per
parse; each one paid two O(env) scans in which every key was resolved to a
`&str` and rescanned for two prefixes. On the benchmark [#7575] measures — a
`.subst(/<?{ rand < .001 }> . /, …)` over 290,000 cursor positions — that pair
cost about as much as everything else the assertion did.

`Env` now keeps an index of exactly those keys, so both halves are
O(routine bindings in scope) instead of O(env) — and zero work for the
overwhelmingly common scope that declares no routines at all.

## How the index stays honest

Three properties make it safe to trust:

- **It is a superset, never a subset.** A removal does not prune it, so a key it
  names may be gone; every consumer reads the key back through
  `Env::overlay_get_sym` instead of assuming it is present. What it must never
  do is *miss* a key that is present, so `insert_sym` and the parent-tier
  promotion inside `get_mut_sym` — the two paths that can add a key to an
  overlay — extend it, and every whole-map rebuild that cannot maintain it
  (`flattened`, `filtered_flat`, `From<HashMap>`, `inner_mut`) resets it to
  "unindexed" rather than carry a stale one forward. The bulk *filters*
  (`retain`, `retain_overlay`, `retain_frame_writes`, `values_mut`) only remove
  keys, so they leave it valid by construction.
- **It costs nothing on the envs that never use it.** Every env starts
  unindexed and stays that way until something asks; the maintenance hook in
  `insert_sym` is one branch on a field that insert already touched. Only
  `Env::code_env_keys` turns the index on, and only a block-scope save calls
  that, so the general insert path never pays the `Symbol::is_code_env_entry`
  memo lookup. `bench-fib` and `bench-ctor` are unchanged.
- **The predicate is one memoized bit.** `flags::CODE_ENV_ENTRY` joins the
  `SymFlags` word in `src/symbol.rs`, fusing `starts_with("&")` and
  `starts_with("__mutsu_callable_id::")` into a single lookup for the index
  rebuild and for `note_code_entry`.

The exit path also stopped using `retain`. `Env::remove_overlay_sym` drops a key
from *this* tier without tombstoning it, which is what `retain`'s filter
semantics were: an enclosing tier's routine binding has to shadow back through,
not be recorded as deleted in this scope. A scope that holds no routine bindings
at all now skips the whole save/restore pair, so it no longer invalidates the
frame-write log or re-derives `?FILE` either.

## Numbers

Release, interleaved A/B against the merge-base binary, 9 runs each, median
(this box is slower in absolute terms than the one that filed the ticket):

| | main | with the index |
| --- | --- | --- |
| `for ^10000 { $seed.subst(/<?{ rand < .001 }> . /, "X", :global) }` | 0.978s | **0.765s** |

That is 22% off the benchmark — slightly more than the 17% the ticket measured
for deleting both halves outright (an unsound experiment), because skipping the
pair also skips `retain`'s `refresh_file_sym` and its frame-write invalidation.
`bench-fib`, `bench-ctor`, `bench-class` and `bench-grammar-parse[-deep]` are
unchanged within noise.

## What is still on the table

The ticket's other measured region is untouched: `eval_regex_inline_code` builds
its per-cursor-position bindings as `String` keys (`i.to_string()`,
`format!("<{}>", k)`, a clone of every `:my` lexical name) and re-interns each
one on the way into the env, which is where the profile's `Symbol::intern` and
malloc traffic come from. Its two *disproven* hypotheses — the registry snapshot
and building `$/` — remain disproven and are recorded on the issue.

`t/block-scope-code-vars.t` pins the language-level invariant the index has to
keep, and `src/env.rs`'s unit tests pin the superset property directly.

[#7575]: https://github.com/tokuhirom/mutsu/issues/7575
