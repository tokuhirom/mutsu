# The closure-call hot path stops rebuilding the same strings every call

`Digest::RIPEMD`'s `t/ripemd.t` ([#7571](https://github.com/tokuhirom/mutsu/issues/7571))
is the one upstream `Digest` file that cannot be whitelisted: the batteries gate
gives each file a hard `timeout 120`, and the 1 MB `'a' x 1_000_000` vector took
longer than that. A fresh profile of the hot loop found that a large share of the
run was not doing arithmetic at all — it was **rebuilding, on every single closure
call, strings and sets whose contents never change**.

## What the profile said

`callgrind` on `rmd160("a" x 4_000)` (release build; instruction counts, which are
deterministic and load-independent, unlike wall clock on a shared container):

| cluster | share of the run |
| --- | --- |
| `Symbol::intern` + its thread-local memo | ~9.3% |
| `format!` machinery (`format_inner` + `core::fmt::write` + `String::write_str`) | ~8% |
| `malloc`/`free` | ~12.6% |

The `format!` traffic alone was 736,961 heap allocations — 21% of every allocation
the program made. Attributing it to callers found five sites, all of them building
a key that is a **pure function of a name that was already interned**:

- the closure exit-path writeback probed `__mutsu_sigilless_readonly::<name>`,
  `__mutsu_sigilless_alias::<name>` and `__mutsu_state_key::<name>` for *every free
  variable of the closure*, on *every* call — three `format!`s per free variable.
  A single `-> [$a, $b]` destructuring parameter anywhere in the program is enough
  to arm that path, and RIPEMD's inner loop is `map -> [&f, $r, @K, $s] {...}`;
- `our_package_var_key` walked up to four candidate packages' `::` chains on every
  `@`/`%` container read once *any* `our` variable exists, `format!`ing a candidate
  key per step and probing a `String`-keyed store with it;
- `get_env_with_main_alias` / `set_env_with_main_alias` built
  `__mutsu_atomic_arr::<name>` / `__mutsu_atomic_hash::<name>` per container access
  once any concurrent array/hash lane exists — which a `start` block that touches a
  shared array arms (this and the placeholder key below were ~1.5% together);
- the same two helpers built a `^<name>` placeholder key per access once any
  placeholder parameter (`$^x`) exists — RIPEMD's round functions are
  `{ $^x +& $^y +| +^$x +& $^z }`;
- the closure-call setup re-interned the literals `&?BLOCK`, `<pointy-block>`,
  `__mutsu_callable_id`, `_`, `$_`, `!`, `@_` and `self` on every call.

## What changed

Nothing about *what* any of these compute — only how often they compute it.

- **Memoized metadata keys.** The three sigilless/state keys, the two atomic-lane
  keys and the placeholder key now go through `Symbol -> Symbol` memos in the shape
  `Interpreter::type_meta_key_for_sym` already established. A name's key never
  changes (symbols are append-only), so the memo needs no generation counter.
- **Memoized `package_qualified_candidate`.** It is a pure function of its two
  strings — no interpreter or registry state feeds it — so a hit can never answer
  for the wrong scope. It returns an interned `Symbol` now, so its callers probe
  with a `&'static str` instead of a freshly allocated `String`.
- **An `our`-variable short-circuit.** `Interpreter::our_var_unqualified` indexes
  every `our_vars` key by its unqualified spelling (`@Foo::Bar::words` -> `@words`).
  Every candidate `our_package_var_key` builds ends in the name's own
  `<sigil><bare>`, so a name absent from that index cannot match any stored key and
  the whole chain walk is skipped. The index is append-only alongside `our_vars`
  (which is only ever inserted into, through one function), so a miss is
  authoritative.
- **Well-known symbols** for the eight per-call literals, via the existing
  `symbol::wk` family, plus `CompiledCode::local_sym` instead of re-interning every
  local's string in the exit-path cleanup loop.
- **Two per-call collections removed.** The writeback scan's `local_names` set is
  now `CompiledCode::capture_local_set()` — the same set, built once per chunk
  behind a `OnceLock` — and its `captured_names` set is now a direct probe of the
  captured env's own map instead of a fresh `FxHashSet` collected from that map's
  keys on every call (~45 entries for a body captured from a wide scope).
- **`Env::entry_or_insert_sym_with`**, a lazy-value twin of `entry_or_insert_sym`.
  The captured-env merge ran the eager form once per captured entry, paying a GC
  refcount bump *and* the matching drop for every entry the caller already had.
- **`merge_sigilless_alias_writes` makes one pass, not two.** Both passes only read
  the current env and write the saved env under per-key-disjoint names, so fusing
  them is observationally identical.
- **`FxHashMap`/`FxHashSet` for three interpreter-internal stores** (`our_vars`,
  `thread_redeclared_vars`, `thread_param_shadow_vars`), which were hashing short
  `String` keys with SipHash on the container-read and closure-call paths. Same
  reasoning as `env::SymMap`.

## Measured

`rmd160("a" x 4_000)`, release, `callgrind` instruction counts:
**4,530,683,849 -> 3,841,376,170 (-15.2%)**. `Symbol::intern` call count fell from
2,653,392 to 2,239,029 even though three of the new memos intern their own inputs.

Wall clock on the session's container (4 cores):

| benchmark | before | after |
| --- | --- | --- |
| `rmd160("a" x 100_000)` | 12.8s | 10.8s |
| upstream `Digest` `t/ripemd.t` (9/9 pass) | 130.7s | 113.7s |

None of this is RIPEMD-specific: every site is on the generic closure-call,
container-read or env-write path, and each one only ever fired at all because the
program used a common Raku construct (a destructuring signature, a placeholder
parameter, an `our` variable, a `start` block).

`t/hot-path-key-memoization.t` pins the behaviour each memo has to preserve —
`our` container resolution through the package chain, sigilless aliases and
attribute reads through closures, `state` variables, the lexical `$_`/`$!`/`self`
of a block called from another routine, `&?BLOCK`, non-local return targeting, and
placeholder binding. It passes unchanged under Rakudo.

`t/ripemd.t` is still NOT whitelisted: 113.7s against a hard 120s budget leaves too
little margin for a slower CI runner, so [#7571](https://github.com/tokuhirom/mutsu/issues/7571)
stays open. The post-change profile has no single dominant item left —
`Symbol::intern` and its memo ~9%, `Env::get_sym`/`contains_key_sym`/`insert`
~13%, `malloc`/`free` ~10% — and the next nameable levers are the parameter
binder (`bind_function_args_values`, ~13% inclusive, ~5.8 interns per bind because
`ParamDef` carries its name only as a `String`) and the `pending_rw_writeback_sources` /
`pending_caller_var_writeback` lists, which are `Vec<String>` re-interned on every
closure return.
