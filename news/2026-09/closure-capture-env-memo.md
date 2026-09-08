# Closure creation stops rebuilding the same captured env every time

Creating a closure literal was O(enclosing env): `capture_closure_env` handed
`Env::filtered_flat` a filter, and `filtered_flat` walked every visible env key
and inserted the kept ones into a brand-new map — per creation. On a
200000-iteration `my $c = * + 1;` loop that one call was **41% of the program's
instructions**, and adding 30 unrelated lexicals to the enclosing scope added
~24ns to every creation ([#7557](https://github.com/tokuhirom/mutsu/issues/7557)
part B).

## What is actually in a capture

Dumping the kept set for a one-line script settled what the issue could only
guess at. A closure created at the top of an otherwise empty program captures
**23 entries**, and 20 of them are the built-in dynamics seeded once at startup:

```
$*ARGFILES $*CWD $*ERR $*HOME $*IN $*OUT $*TMPDIR %*ENV
*ARGFILES *CWD *ERR *HOME *IN *OUT *PROGRAM *PROGRAM-NAME *REPO *SCHEDULER
*TMPDIR @*ARGS =pod ?FILE Any
```

None of them is a free variable of any closure; they are kept because they are
not *plain user lexicals* (`env::is_plain_user_lexical`), which is the
deliberately conservative rule that keeps dynamics, magic vars, `self`, type
names and `__mutsu_*` metadata reachable from a closure body. So the filter
result is not merely expensive to compute — it is the *same* result over and
over.

## The memo

`filtered_flat`'s filter here reads only the KEY, never the value, so the
captured map is a pure function of the visible env contents and of the closure's
own `CompiledCode`. `src/vm/vm_capture_cache.rs` memoizes exactly that: one
entry, holding the env tiers it was built from, the chunk, and the resulting
env. A creation whose inputs are unchanged clones the memoized `Env` (an `Arc`
bump) instead of rebuilding the map; everything that varies per creation — the
free-var upvalue reads from the frame's live slots, the ADR-0024 mainline
lexical overrides, `capture_bare_callees`, the `self` materialization — still
runs afresh, factored out into `finish_closure_capture`.

**Why comparing tier addresses is sound.** An armed entry holds an `Arc` on
every tier's overlay map. That both keeps the allocations from being freed and
recycled at the same address *and* pushes `Env::cow_mut`'s `Arc::make_mut` onto
its cloning path, so any by-name write moves the written tier to a new address.
Matching addresses therefore prove the contents are the ones the entry was built
from. A chain carrying tombstones is refused outright (a tombstone set is a
plain `FxHashSet` the memo cannot pin). A write *through* a captured
`ContainerRef` cell is deliberately not a mismatch: the memo holds the same
cell, exactly as a freshly built capture would. `t/closure-capture-memo.t` pins
the observable semantics and two `env.rs` unit tests pin the address invariant
itself.

**Why arming waits for a repeat.** Holding those `Arc`s costs one
copy-on-write clone on the next write to a pinned tier. An entry is therefore
armed only after two consecutive captures have seen the same chain and chunk: a
scope that churns is tracked with bare addresses (nothing held, nothing pinned)
and never pays for a memo that could not hit. The cache itself is boxed on the
`Interpreter` alongside `cur_repo` — inlining ~180 bytes of closure-only state
would push the per-opcode hot fields apart for every program.

## Also in this change

Fixed per-creation costs on the same path, all semantics-free:

- The capture filter asked two string questions about every visible env key
  (`is_plain_user_lexical`, `is_attr_twigil_env_key`) by resolving the symbol and
  re-scanning its bytes, twice per key. Both are now bits in the memoized
  `Symbol::flags()` byte, so the filter is one thread-local lookup per key.
- `Symbol::intern(&self.lexical_closure_package())` allocated a `String` and
  re-hashed it per creation just to intern it. `lexical_closure_package_sym()`
  answers with the `Symbol` directly; the common case (no method frame in the
  way) is an atomic load.
- `materialize_frame_self_into_capture` re-interned `"self"` and allocated a
  `String` key per creation; it is symbol-keyed now.

## Result

Deterministic instruction counts (callgrind, load-independent — the wall-clock
rows will come from the bench CI):

| | before | after | |
|---|---|---|---|
| `my $c = * + 1;` × 200000 | 4,561,796,171 | 2,667,115,037 | **−41.5%** |
| the same with 30 extra enclosing lexicals | 6,924,565,867 | 3,224,306,708 | **−53.4%** |
| `bench-ctor` | 1,481,775,564 | 1,462,503,952 | −1.3% |
| `bench-class` | 1,246,662,130 | 1,245,785,315 | −0.1% |
| `word-count` | 1,132,877,066 | 1,132,704,822 | −0.0% |
| `bench-fib` | 1,408,905,100 | 1,408,911,125 | +0.0% |
| `bench-tak` | 1,682,291,882 | 1,682,305,527 | +0.0% |

The scaling that named the issue is what moved most: those 30 unrelated
lexicals used to add ~24ns to every creation and now add ~4ns, because a memo
hit no longer touches the enclosing env at all.

## Still open

The kept set is still over-broad by construction, and a capture that *misses*
the memo still pays O(kept env). Narrowing the rule — deciding which of those
names are genuinely read by runtime by-name mechanisms and which only through a
compiled `GetLocal`/`GetGlobal` the free-var pass already sees — remains the
design question #7557 part B poses, and the dump above is the evidence a future
pass should start from: the answer is mostly about the built-in dynamics.
