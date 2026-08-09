# Sibling threads' distinct `my @a` bindings merge through the root-scoped `__mutsu_atomic_arr::` name lane

## Affected tests
- `t/http-session-inmemory.rakutest` subtests 8, 9 ("No session confusion with concurrent clients (A)/(B)")
- `t/http-session-persistent.rakutest` subtests 8, 9 (same names)

Observed failure shape: `$res-a` = `'Visit 1,Visit 1,Visit 2,...,Visit 8'` (9 elements) and `$res-b` = same prefix + one more element (10 elements) instead of two independent `'Visit 1..5'` strings. `$res-b` being exactly `$res-a` plus one trailing element proves both `start` blocks pushed into ONE array. The visit numbers running 1,1,2..9 (a single server-side counter) is the same bug hitting the cookie jar's internals: `Cro::HTTP::Client::CookieJar!purge` builds `my @replacer` and `add-to-request` builds `my @cookie-list` (plain lexicals, run concurrently by both clients' request pipelines), so the two jars' cookie lists merge, both clients start sending the same session cookie, and the two sessions collapse into one.

## Repro (verified)
`tmp/repro-start-await-r4.raku` — no Cro needed:

```raku
sub fetch($n, $i) { start { sleep 0.05; "$n$i" } }
my $pa = start {
    my @a;
    for 1..5 -> $i { push @a, await fetch('A', $i) }
    @a.join(',')
}
my $pb = start {
    my @a;
    for 1..5 -> $i { push @a, await fetch('B', $i) }
    @a.join(',')
}
say "A: ", await $pa;
say "B: ", await $pb;
```

- raku: `A: A1,A2,A3,A4,A5` / `B: B1,B2,B3,B4,B5`
- mutsu (release): `A: A1,B1,A2,B2,A3,B3,B4,A4,A5` / `B: A1,B1,A2,B2,A3,B3,B4,A4,A5,B5`

Negative controls that PASS (pin these to bound the fix):
- Same loop with no nested spawn inside the threads (`push @a, "$n$i"; sleep 0.05` — no `await fetch`): no merge (`tmp/repro-start-my-array.raku`).
- Scalar accumulator `my $acc = ''; $acc ~= $v` instead of `my @a`/push: no merge (`tmp/repro-start-await-r3.raku`).

## Root cause
Chain of three mechanisms, all by design individually, colliding:

1. `my @a` inside a worker thread masks the name in `thread_redeclared_vars` (`src/vm/vm_var_assign_set_local.rs:1928-1945`), keeping it frame-local.
2. **Any nested spawn drops that mask.** `clone_for_thread_excluding` force-`declare`s each re-declared name into the lineage store and then `retain`s away the mask (`src/runtime/runtime_thread.rs:241-249` and `:301-305`). The nested spawn here is the inner `start` in `fetch` (in Cro: every `$client.get` spawns). After that spawn, `container_name_is_redeclared("@a")` (`src/runtime/runtime_shared_vars.rs:210`) is false in that thread.
3. With the mask gone, `ArrayPush` on a worker thread routes the push through the name-keyed atomic lane: gate at `src/vm/vm_data_push_ops.rs:123-135` (`is_thread_clone() || array_name_is_shared(...)`) → `shared_array_extend` (`src/runtime/builtins_atomic_shared.rs:253`). The lane key `__mutsu_atomic_arr::@a` is an *internal* key, and internal keys resolve at the **root store**, not the declaring lineage: `SharedStore::get`/`scope_for` in `src/runtime/shared_store.rs:106-114`, and `atomic_array_entry_exists` explicitly reads `self.shared_vars.root_store()` (`src/runtime/builtins_atomic_shared.rs:297-309`).

So thread A's `@a` and thread B's `@a` — two unrelated frame-local bindings that merely share a bare name — both funnel into ONE process-global `__mutsu_atomic_arr::@a` entry, and each thread's final `@a.join(',')` reads the merged array. The ADR-0010 lineage chaining protects the base-name entries (`declare` binds into the current lineage) but the `__mutsu_atomic_arr::` lane bypasses it by being root-scoped.

## Fix direction
Preferred surgical fix: **scope the `__mutsu_atomic_arr::` / `__mutsu_atomic_hash::` keys to the lineage that owns the base name** instead of the root store. Concretely:
- In `src/runtime/shared_store.rs`, make `scope_for` for `__mutsu_atomic_arr::@a` / `__mutsu_atomic_hash::%h` resolve to `owner_of("@a")` (the lineage tier that owns the base name via the nested-spawn `declare`), falling back to root only when no lineage owns the base name. `is_internal_key` currently sends all `__mutsu_*` keys to root; the atomic-array/hash lanes need the base-name-owner rule, while genuinely process-global internals (`__mutsu_shared_state::`, atomic-name lanes used by `cas`) keep root scoping.
- Audit the direct `root_store()` reads: `atomic_array_entry_exists` (`src/runtime/builtins_atomic_shared.rs:297`) and the other `__mutsu_atomic_arr` sites listed by `git grep __mutsu_atomic_arr src/` (runtime_shared_vars.rs:130/472/507/572, runtime_thread.rs:765/877, vm_var_assign_local_get.rs, vm_env_helpers.rs, vm_call_method_mut_ops.rs) — they must all use the same owner-lineage resolution or reads/writes will split brain.

Alternative (bigger, the ADR-0001/Track-B answer): give each `my @a` binding a per-binding `ContainerRef` cell and stop keying concurrency by name entirely. That is the long-term architecture; don't start it for this ticket.

Do NOT "fix" this by keeping the redeclared mask across nested spawns — the force-declare + unmask exists to let genuinely shared re-declared names propagate (see the long comment at runtime_thread.rs:268-305); reverting it regresses `$port`/`$tap` bugs documented there.

Risks: the lane is shared infrastructure for `t/lock.t`-style genuinely-shared arrays (parent declares `@a`, threads push) — that case still works because the parent's lineage owns the base name and all children resolve to it. Run `t/lock.t`, `t/thread-shared-scalar-visibility.t`, roast S17 whitelist files locally.

## Verification
- `tmp/repro-start-await-r4.raku` and `tmp/repro-start-await-r1.raku` print disjoint A/B sequences.
- `t/http-session-inmemory.rakutest` subtests 8-9 pass (13/13 together with the other session tickets).
- `t/http-session-persistent.rakutest` subtests 8-9 pass.
- Negative controls above still pass; `make test` + CI roast green.
