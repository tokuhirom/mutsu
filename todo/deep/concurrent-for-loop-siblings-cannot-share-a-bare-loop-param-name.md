# Two concurrent `for LIST -> $x { start {...} }` sibling iterations of a non-"plain" value collide on the bare-name shared-store lane

> **Design settled (2026-08-10):** `docs/adr/0023-binding-provenance-spawn-capture.md`
> — a third path, smaller than either option (a)/(b) below: track active
> for-loop parameter names and have `block_captured_scalars` treat them as
> closure-owned regardless of value type, so each spawn's env clone (already
> correct per iteration) is never seeded into or overwritten by the bare-name
> lane. The ADR contains the full implementation plan and acceptance criteria;
> implement from there, not from the (a)/(b) sketches below.

## TL;DR

`for $client-a, $client-b -> $client { start { ... $client ... } }` — a
single-param `for` loop whose body spawns a `start` block per iteration and
whose loop items are NOT one of `block_captured_scalars`' "plain" scalar
types (Int/Num/Str/Bool/Rat/.../ContainerRef) — cannot represent the two
concurrently-live iterations' distinct `$client` bindings. Both spawned
threads eventually converge on reading whichever iteration's value most
recently won a last-writer-wins race on a single bare-name shared-store slot,
instead of each thread keeping its own. This is a genuine architectural gap
in the cross-thread bare-name lane (PLAN.md §6 / ADR-0010), not a one-line
bug: **masking (`thread_redeclared_vars`, the same mechanism multi-param
`for` loops already use) does NOT fix this**, because masking only decides
which of two equally-wrong branches (force-overwrite vs. seed-once-then-
ignore) a spawn takes — neither can represent two *simultaneously live*
values under one bare-name key.

## Minimal, portable, cross-verified repro (no Cro needed)

`tmp/repro-minimal-given-barename.raku`:

```raku
class Widget {
    has $.id;
}

# Warm-up: a `given EXPR -> $client { ... await ... }` block that fully
# completes before the concurrent for-loop below even starts.
given Widget.new(id => 'warmup') -> $client {
    await Promise.in(0.01);
    say "warmup done, client={$client.id}";
}

my $client-a = Widget.new(id => 'A');
my $client-b = Widget.new(id => 'B');

my @promises = do for $client-a, $client-b -> $client {
    start {
        my @a;
        for 1..5 -> $i {
            await Promise.in(0.01);
            @a.push($client.id);
        }
        @a.join(',');
    }
}
say (await @promises).join(' | ');
```

- `raku`: `A,A,A,A,A | B,B,B,B,B` (correct).
- `mutsu` (as of `b67c54dcd`, main): `B,B,B,B,B | B,B,B,B,B` (both threads
  converge on client B).

Three companion files pin the exact trigger conditions:

- `tmp/repro-minimal-given-barename-no-warmup.raku` — same file minus the
  warm-up `given` block. **Correct** (`A,A,A,A,A | B,B,B,B,B`). So the bug
  needs *some* prior, fully-completed, unrelated use of the bare name
  `client` before the concurrent for-loop begins — it is not simply "two
  concurrent same-named for-loop iterations are always broken".
- `tmp/repro-minimal-given-barename-renamed.raku` — same file, warm-up
  parameter renamed `$warmup` instead of `$client`. **Correct.** Confirms the
  trigger is specifically the *warm-up and the for-loop's bound parameter
  sharing the same bare name* `client`.
- `tmp/repro-minimal-plain-block-barename.raku` — same shape, but the
  warm-up is a **plain block-scoped `{ my $client = ...; await ...; }`**,
  not a `given`. **Also reproduces.** So the trigger is not `given`-specific;
  any block-scoped `my $NAME` (or pointy-block binding of the same name)
  that runs to completion once, anywhere earlier in the program, before a
  LATER, unrelated `for LIST -> $NAME { start {...} }` with the same bare
  name, is sufficient.

All four were also run against `raku` to confirm expected behavior
(`A,A,A,A,A | B,B,B,B,B` in every case — raku has no such bug).

## Original discovery context (Cro campaign)

Found via `t/http-session-inmemory.rakutest` / `t/http-session-persistent.rakutest`
subtests 8-9 ("No session confusion with concurrent clients"), which use
exactly this shape:

```raku
given Cro::HTTP::Client.new -> $client {          # <- warm-up, subtests 1-2
    given await $client.get("$url/hits") { ... }
}
given Cro::HTTP::Client.new(:cookie-jar) -> $client { ... }   # <- subtests 3-7

given Cro::HTTP::Client.new(:cookie-jar) -> $client-a {
    given Cro::HTTP::Client.new(:cookie-jar) -> $client-b {
        my ($res-a, $res-b) = await do for $client-a, $client-b -> $client {
            start {
                for 1..5 -> $i {
                    given await $client.get("$url/hits") { ... }
                }
            }
        }
    }
}
```

The very first `given Cro::HTTP::Client.new -> $client {...}` warm-up block
(subtest 1's client, a **third, unrelated** `Cro::HTTP::Client` instance) is
what poisons the later concurrent block's `$client` — confirmed by isolating
it down to `tmp/repro-warmup1single-concurrent.raku` (warmup1 alone,
reproduces) vs `tmp/repro-warmup-generic-async.raku` (an unrelated
`await Promise.in(0.01)` warm-up with no `Cro::HTTP::Client`/no same-named
`given`, does NOT reproduce) vs `tmp/repro-warmup-renamed-param.raku`
(warmup's parameter renamed, does NOT reproduce). This is what narrowed the
bug down to the minimal, Cro-free repro above.

Direct evidence from `CRODBG=1`-instrumented `tmp/shadow/lib/Cro/HTTP/Client.rakumod`
(add `note "DEBUG get self.WHICH=..." if %*ENV<CRODBG>;` to `multi method
get($url, *%options)`) against `tmp/repro-session-full.raku` (a copy of the
real test with `note "  inner iter $i client={$client.WHICH}"` added inside
the concurrent block's inner `for` loop): from inner iteration 2 onward, BOTH
threads' own `for 1..5 -> $i { note "...{$client.WHICH}..." }` line — reading
`$client` directly, no method call involved — printed the SAME
`Cro::HTTP::Client` instance (client B's), even for what should have been
client A's thread. This is not a Cro-internal bug at all; it is the `for`
loop's own bound-parameter identity that is wrong.

## Root cause (confirmed, not speculative — traced with two throwaway
## debug-instrumented builds, both reverted before this ticket)

1. **Single-param `for LIST -> $x { ... }` binds `$x` via a direct,
   un-gated env write** (`vm/vm_for_loop_body.rs:438`,
   `self.env_mut().insert(name.clone(), item.clone());`), NOT through
   `exec_set_var_dynamic_op` (the `my`-declaration opcode that inserts into
   `thread_redeclared_vars` — see `vm/vm_var_assign_set_local.rs:1928-1945`).
   Confirmed empirically: an `eprintln!` gated at
   `exec_set_var_dynamic_op`'s entry for `name == "client"` fires exactly
   ONCE per run of the minimal repro (for the warm-up's `my $client`/pointy
   bind), never for the for-loop's own `-> $client` — the existing code
   comment at `vm_for_loop_body.rs:279` ("The single-param form binds
   natively and never had this problem") is accurate about the *mechanism*
   but wrong about the *consequence*: masking was never needed to protect
   against `set_shared_var_sym`-style publishing (single-param doesn't use
   `Stmt::Assign` the way `build_for_bind_stmts` does for multi-param), but
   the plain env write is STILL visible cross-thread through a different
   path (see next point), and that path has no per-iteration isolation.

2. **`block_captured_scalars`** (`runtime/runtime_thread.rs:13-101`) decides,
   for a spawned `start {}` block, which of its free variables the closure
   machinery itself owns (a per-binding `ContainerRef` cell from
   `box_captured_lexicals`, or a frozen-by-value copy) versus which stay on
   the cross-thread **bare-name shared-store lane** as the only visibility
   mechanism. The "plain" allow-list (`runtime_thread.rs:48-61`) is
   `Int/BigInt/Num/Str/Bool/Rat/FatRat/BigRat/Complex/ContainerRef` — an
   **`Instance`** (any user class object, `Cro::HTTP::Client` in the real
   case, `Widget` in the minimal repro) is NOT on that list. The function's
   own comment documents this as a **known, deliberate** limitation: "Only a
   genuinely PLAIN scalar is owned per binding by the closure machinery...
   everything else (a Channel, a Promise, a Lock, an Array/List/Hash, a Sub,
   a type object, ...) is a shape `box_captured_lexicals` declines to box, so
   the name lane is still the only thing keeping the parent and the worker
   on ONE object."

3. **The bare-name lane cannot represent two *simultaneously live*, distinct
   values under one name.** `clone_for_thread_excluding`
   (`runtime/runtime_thread.rs:130-328`) seeds each spawn's current value
   into the shared lineage store via one of two branches at line ~264-272:
   - If `thread_redeclared_vars` contains the name (and it's not
     in-flight/param-shadowed): **force `shared.declare(key, val)`** —
     unconditionally overwrites whatever is there.
   - Otherwise: **`shared.seed_if_absent(key, val)`** — writes only if no
     lineage in the ancestor chain already has the name.

   Neither branch is correct when client-a's spawn and client-b's spawn
   both need to publish `client` under the SAME bare name at roughly the
   same wall-clock moment: `seed_if_absent` makes the SECOND spawn's seed a
   no-op (the traced run showed exactly this: client-a's spawn seeds
   `client=A` since the key was absent; client-b's spawn's `seed_if_absent`
   returns `false` because the key now exists), and — empirically verified
   with a throwaway patch — extending the multi-param `for` loop's own
   `thread_redeclared_vars` masking to also cover the single-param case (so
   BOTH spawns take the force-`declare` branch instead) does not fix it
   either: it just changes who clobbers whom (now BOTH spawns unconditionally
   overwrite the single shared slot, and the later spawn — still,
   deterministically, client B in testing — wins for both threads for the
   rest of the run). **This confirms the fix is not "which branch a spawn
   takes" — it is that a name-keyed store fundamentally cannot hold two
   concurrently-live bindings of one bare name.** (That masking patch was
   reverted; it is not present in the tree. See "What was tried" below if
   picking this up.)

4. Why the **warm-up matters**: with no prior use of `client` anywhere, the
   store never held a `client` entry, so a race between two *first* writers
   — while still a race — happened to land closer to correct in observed
   testing (though this is NOT provably safe either, just apparently not
   the common failure mode without a third contending write). Once a THIRD,
   unrelated, fully-sequential prior binding of the same bare name (the
   warm-up) has *also* touched `client` — via `exec_set_var_dynamic_op`
   inserting into `thread_redeclared_vars` (when `self.shared_vars_active`,
   which itself only becomes true after the FIRST ever thread spawn in the
   process — for the Cro case that's already true by the time the warm-up
   runs, since `Cro::HTTP::Server.new(...).start` spawns background threads;
   for the minimal repro, the warm-up's OWN `await Promise.in(...)` is what
   flips `shared_vars_active` true, via the scheduler's internal `cue`
   thread spawn) — the interaction between that stale mask state, the
   ancestor-chain lookup in `SharedStore::contains_key`
   (`runtime/shared_store.rs:164-169`, which walks up `self.parent`), and
   which of the two concurrent spawns' writes lands in which lineage
   changes the specific outcome, but the underlying defect (one bare name,
   two live values) is present with or without a warm-up — the warm-up only
   changes which failure shape is observed.

## Why this is `todo/deep/`, not a quick ticket

A correct fix needs one of:

- **(a) Extend closure capture (`box_captured_lexicals` /
  `block_captured_scalars`'s "plain" allow-list) to cover Instance values
  (and, ideally, all types)**, so a `start {}` block's captured loop
  parameter always gets its own per-binding cell regardless of type, making
  it fully independent of the bare-name lane. This is the architecturally
  clean fix, but `block_captured_scalars`'s own comment implies the
  exclusion of "everything else" is deliberate, not an oversight — likely
  because boxing an Instance (or Array/Hash/Sub/Channel/Promise/Lock) into a
  fresh `ContainerRef` cell risks changing identity/mutation-sharing
  semantics for code that currently relies on the raw value flowing through
  unwrapped. This needs careful design (which types are safe to box, whether
  a `ContainerRef`-wrapped Instance behaves identically to the raw Instance
  at every call site that pattern-matches on `ValueView`, etc.) — likely its
  own ADR, in the same family as ADR-0001's Track B / `Gc<T>` container-kind
  work already flagged as high-blast-radius and deliberately sequenced.
- **(b) A per-lineage-isolated name lane** that can distinguish sibling
  concurrent bindings of the same bare name by their spawn's own lineage
  identity rather than a single flat/ancestor-chained key — effectively
  making `for`-loop-spawned siblings each get their OWN child lineage for
  their loop parameter instead of sharing the loop's enclosing lineage. This
  changes `clone_for_thread_for_block`'s relationship to the *iteration*
  rather than just the *call*, and interacts with the existing
  `thread_param_shadow_vars` / `thread_redeclared_vars` machinery in ways
  that need the same careful, whole-mechanism review the multi-param `for`
  fix (`#6081`) and the slurpy-param fix (`#6173`) each required.

Either path touches core cross-thread variable-visibility machinery
(`runtime_thread.rs`, `runtime_shared_vars.rs`, `shared_store.rs`) that this
campaign has already had to fix many times (see MEMORY.md's "Slice F /
dual-store" section and the many PRs referenced there) — this is exactly the
kind of substantial, high-blast-radius change CLAUDE.md's "Refactor boldly"
section calls for, not a one-file patch, and needs `make test` + a full
`make roast` CI run to validate it doesn't regress any of the many other
`thread_redeclared_vars`/`shared_vars` fixes already landed.

## What was tried and reverted (do not re-attempt without a new idea)

1. Removing `thread_redeclared_vars` entries for a plain block's own `my`
   declarations at `BlockScope` exit (`vm/vm_misc_scope.rs`,
   `exec_block_scope_op`), mirroring `unmask_for_multi_params`. **Did not
   fix it** — turned out to be irrelevant, since the warm-up's `my $client`
   never inserts into `thread_redeclared_vars` in the first place (that
   insert is gated on `self.shared_vars_active`, and — depending on the
   exact repro — timing means the warm-up path sometimes runs before
   `shared_vars_active` ever flips true). Reverted; not present in the tree.
2. Extending `masked_multi_params` in `vm/vm_for_loop_body.rs` (the
   multi-param `for` loop's `thread_redeclared_vars` masking, `#6081`) to
   also cover the single named `param_name`, via
   `spec.multi_param_names.iter().chain(param_name.iter())`. **Did not fix
   it** — see point 3 in "Root cause" above: this only changes which
   overwrite-losing branch a spawn takes, and two concurrently-live values
   under one name lose either way. Reverted; not present in the tree.

## Verification (once a real fix lands)

- `raku`/`mutsu` output parity on all four `tmp/repro-minimal-given-barename*.raku`
  files above (currently only the `-no-warmup` and `-renamed` variants pass).
- `t/http-session-inmemory.rakutest` subtests 8-9: `Visit 1,2,3,4,5` for both
  A and B (not interleaved), and `t/http-session-persistent.rakutest`
  subtests 8-9 likewise.
- `make test` + full `make roast` CI (this touches shared cross-thread
  variable machinery broadly enough that a local subset run is not
  sufficient reassurance — see CLAUDE.md's "Delegate the full roast run to
  CI").
