# The Real/Numeric native-dispatch probe is memoized per class

Before it will decline a method, `try_native_method_raw` asks of **every**
instance receiver whether that receiver must route through the interpreter's
Numeric bridge: does it match `Real`, or `Numeric`, or does its class provide a
`Bridge` method? Asked inline, that was two full `type_matches_value` walks per
instance method call — each one walking the receiver class's MRO *and* the
transitive closure of its composed roles — plus a method lookup. On the
`Buf.push` loop in #7712 that single call site dominated the profile, and
nothing about it was `Buf`-specific: the probe runs for any instance receiver
whose class `is_native_method` declines.

The probe's answer is a property of the **class**, not of the call. Given a
fixed registry, "does an instance of `C` match `Real`/`Numeric`, or have a
`Bridge` method" cannot differ between two calls with the same receiver class,
so it wants memoizing per class symbol. The obstacle #7712 named was
invalidation, not the cache: the answer is derived from `registry.classes`
(parents and MRO), `registry.class_composed_roles`, `registry.role_parents` and
`registry.subsets`, and `Registry::method_generation` covers none of them. A
hand-enumerated invalidation list over the ~30 sites that mutate those five
maps is exactly the "correct only under an incomplete static analysis" shape
CLAUDE.md rules out — a site missed today, or added next month, would not fail
loudly, it would silently serve a wrong type answer.

**The choke point turned out to already exist.** All five maps live behind
`Interpreter::registry_mut()`, the sole write path to the shared
`Arc<RwLock<Arc<Registry>>>`, and that accessor already bumps
`registry_write_gen` on *acquisition* — a counter several resolution caches
(regex code parsing, routine resolution) consult for exactly this reason. Keying
the memo on it is sound *by construction* rather than by enumeration: a registry
write that does not bump the generation is not expressible, so a mutation site
added later is covered with no edit to the cache. It over-invalidates — a
method-table write drops type-relation answers too — which costs one rewalk per
class per registry write and never a wrong answer.

Two things the generation deliberately does not cover are handled explicitly in
`src/runtime/numeric_bridge_probe.rs`:

- A user `subset Real` / `subset Numeric` shadowing the builtin name makes the
  probe run a `where` predicate. That is user code, whose side effects roast
  counts (`S12-subset/subtypes.t`) and whose answer may legitimately differ
  between two calls, so the memo switches itself off entirely while either name
  is shadowed.
- "The answer depends only on the class" is a property of `type_matches_value`'s
  instance arms, not of the registry, and no generation can police it. Debug
  builds therefore re-derive the answer on every cache hit and assert it matches
  the memoized one, so a future arm that starts reading the *instance* fails
  loudly instead of silently serving a stale answer — the loud-failure property
  the hand-enumerated route could not offer. `make test` drives `t/` with the
  release binary, so the tripwire costs CI nothing; the whole suite was run
  against `target/debug/mutsu` with it armed (3902 files, 41466 tests, green)
  as evidence for the class-only property it guards.

`t/numeric-bridge-probe-memo.t` pins both halves: a class that reaches `Real`
directly, through a composed role and through a parent keeps bridging on every
call (not just the first), a plain neighbour class does not acquire a `Bridge`
from the memo, and a `Bridge` method added by a runtime `augment` — after the
class has already been probed and cached — is still seen.

The speedup itself is recorded by the bench CI row for the merge commit; see
#7712 for the callgrind attribution that motivated the work.
