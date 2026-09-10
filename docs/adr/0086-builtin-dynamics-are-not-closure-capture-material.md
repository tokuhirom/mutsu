# ADR-0086: The built-in dynamics are not closure-capture material

- Status: Proposed
- Date: 2026-09-10
- Related: [ADR-0084](0084-the-frame-env-is-not-the-programs-symbol-table.md)
  (the per-frame `Env` is not the program's symbol table) — this ADR refines the
  one group ADR-0084 §2 deliberately leaves in the `Env` ("lexical variables
  *and dynamics*"); [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md)
  (slot-addressed lexical capture, which is why the *lexical* half of a capture
  is already O(free variables)); and
  [ADR-0055](0055-closure-free-vars-resolve-to-their-own-binding.md) (a free
  variable resolves to the binding the closure captured).
- Addresses: [#7557](https://github.com/tokuhirom/mutsu/issues/7557) Part B —
  "the kept set is over-broad by construction, and a capture that misses the
  memo still pays O(kept env)".

## 1. Context

### 1.1 What a closure capture actually contains

`Interpreter::capture_closure_env` (`src/vm/vm_register_ops.rs`) builds a
closure's captured `Env` by walking every visible env key and keeping

```
free_var_syms  ∪  { k : k is not a plain user lexical }
```

The second half is deliberately conservative. mutsu stores scalars sigil-less,
so a user's `my $Foo` and a bare type name `Foo` are the same env key *shape*;
`env::is_plain_user_lexical` can therefore only answer "droppable" for a
lowercase-identifier-shaped key, and everything else — dynamics, magic vars,
`self`, `?CLASS`, `__mutsu_*` metadata, and as collateral every uppercase-initial
user lexical — is kept.

Dumping a capture from a closure at the top of an otherwise empty program gives
**23 entries, 20 of which are the built-in dynamics seeded once at interpreter
startup** (#7557, 2026-09-08):

```
$*ARGFILES $*CWD $*ERR $*HOME $*IN $*OUT $*TMPDIR %*ENV
*ARGFILES *CWD *ERR *HOME *IN *OUT *PROGRAM *PROGRAM-NAME *REPO *SCHEDULER
*TMPDIR @*ARGS =pod ?FILE Any
```

(each dynamic is seeded under both its `$*X` and its sigil-less `*X` spelling,
which is a redundancy of its own). ADR-0084's much wider census of a
`use Cro::HTTP2::RequestParser` frame — 403 entries — counts the same 20
dynamics; what changes with program size is the type-name and marker groups
ADR-0084 owns, not this one.

### 1.2 What it costs

The dynamics are rebuilt, identically, on every closure creation.
`Env::filtered_flat` walks the tier chain and inserts each kept entry into a
fresh `SymMap`; each kept entry costs a filter call, a `HashMap::insert`, a
`Value` clone (a GC refcount bump), and later the matching drop when the closure
dies.

#7624 memoized the whole result one entry deep, which removes that cost for a
closure literal re-created from an *unchanged* scope. It cannot help a closure
created inside a sub or method frame, whose env tiers are fresh on every call —
and that is the common shape (`.map({…})` in a method, `bench-ctor`'s `*.flat`
inside `TWEAK`). Callgrind on a 20000-iteration
`sub make($n) { my $c = { $n + 1 } }` loop, after the #7847 baseline:

| | Ir per creation | share of the program |
| --- | --- | --- |
| `capture_closure_env` (inclusive) | 13,489 | 21.1% |
| of which `filtered_flat` | 7,784 | 12.2% |
| of which the memo's own bookkeeping | 2,871 | 4.5% (at a 0% hit rate — see §5) |

`filtered_flat`'s 7,784 is spent on 31 filter calls and 28 inserts, of which the
20 built-in dynamics are the bulk and are byte-identical every time.

### 1.3 The finding that reframes the question

The issue framed Part B as "decide that free-variable analysis is authoritative
for some class of names it currently is not trusted for" — an incomplete-static-
analysis risk of exactly the shape CLAUDE.md warns about. Reading the *call*
side changes the question.

When a closure is called, `call_compiled_closure_in_unit`
(`src/vm/vm_closure_dispatch.rs`) installs an empty overlay over the **live
caller env** and merges the captured entries into it with
`entry_or_insert_sym_with` — *don't overwrite what the chain already has*
(the handful of exceptions are `ContainerRef` cells, `self`, and a non-routine
block's `$_`/`$!`; a captured dynamic is explicitly excluded from the
`ContainerRef` overwrite, because a dynamic's live value is whatever the current
dynamic frame set).

So a captured entry can only ever win where the live chain *lacks* the key. The
built-in dynamics are seeded into the interpreter's root env and live there for
the whole program, and every frame chain bottoms out at that root, so the
captured copy of `$*OUT` is discarded on every single call. **The 20 entries are
not an over-broad approximation that happens to be harmless — they are dead
weight that is already never read.**

What the capture *does* legitimately hold for this family is a **user's**
dynamic: `my $*CWD = …` inside a block that has since exited is genuinely absent
from the live chain, and the captured copy is the only surviving binding
(`t/start-dynamic-var-indir.t`, `roast/S32-io/indir.t`). Those are exactly the
entries whose value is *not* the one the interpreter seeded.

## 2. Decision

**Proposed.** The built-in dynamics are interpreter state, not closure-capture
material. They move into a **per-interpreter, never-copied env tier** — the
mutable sibling of the existing `GLOBAL_BASE` — which every env in that
interpreter reads through and no capture, frame clone, or thread clone
materializes. A closure's captured env then holds a dynamic's key only when the
program bound one, which is the only case the capture was ever read for.

## 3. Why the existing mechanisms cannot take them

mutsu already has the shape this asks for, and it already holds five of these
names — which is why the remainder needs a decision rather than a patch.

- **`GLOBAL_BASE` + `IMMUTABLE_BASE_DYNAMICS`** (`src/runtime/mod.rs`,
  `runtime_init.rs`) already hoists `$*PID`, `$*TZ`, `$*INIT-INSTANT`,
  `$*EXECUTABLE`, `$*EXECUTABLE-NAME` and `$*SPEC` out of every overlay into a
  never-copied tier, in both spellings. It cannot take the rest: `GLOBAL_BASE` is
  a process-wide `OnceLock`, and `$*OUT`/`$*ERR`/`$*IN`/`$*ARGFILES` are
  per-interpreter handles, `$*PROGRAM`/`$*PROGRAM-NAME`/`@*ARGS` are per-
  interpreter (Test::Util's `is_run` fast path runs a nested `Interpreter` in the
  same process), and `$*CWD`/`$*REPO`/`$*SCHEDULER` are mutable. Only `$*HOME`
  and `$*TMPDIR` are genuinely process constants, and hoisting those two buys 4
  entries of 23.
- **`Env::scoped_child`** would let a capture keep the dynamics by reference
  instead of by value, at one `Arc` bump. ADR-0084 §5 already rejects this for
  long-lived captures and the rejection stands here: `Env::iter`/`keys`/`values`/
  `len` are overlay-only, so a closure env with a parent starves every
  iteration consumer (`$DYNAMIC::` / `CALLER::` collection, the pseudo-stashes,
  the END-phaser watch, the call-time merge itself, which iterates
  `data.env.iter()`).

A per-interpreter tier avoids both: it is not process-wide, so per-interpreter
values are fine; and it is a *base* tier consulted at the chain's tail like
`GLOBAL_BASE`, not a parent overlay, so it does not change what a chain walk
means.

## 4. Invariants

- **A user-bound dynamic still captures.** `my $*CWD = …` / `indir` / a `temp
  $*OUT` bind into an ordinary overlay tier, shadow the base, and are captured
  exactly as today — the escaping-closure cases in `t/start-dynamic-var-indir.t`
  and `roast/S32-io/indir.t` are the acceptance test for this ADR, not a
  casualty of it.
- **A write to a built-in dynamic is promoted, not applied to the base.** The
  base tier is immutable for the interpreter's life; `$*CWD = …`, `chdir`,
  `indir` and `$*TMPDIR = …` write an overlay entry that shadows it. This is the
  same promotion `IMMUTABLE_BASE_DYNAMICS` already relies on.
- **Iteration still sees them.** Whatever iterates an env as "the visible
  environment" — `$DYNAMIC::`/`CALLER::` collection
  (`runtime_caller_env.rs`), the pseudo-stashes, `flattened()` — must be taught
  the base tier, or it loses `$*OUT`. This is the largest single piece of the
  work and the reason it is not a drive-by.
- **Thread clones keep their own.** `init_io_environment_for_thread_clone`
  inherits a redirected `$*OUT` and rebuilds the rest per thread
  (`t/start-inherits-dynamic-out.t`); a per-interpreter tier is per thread-clone
  interpreter, so this is preserved by construction rather than by copying.
- **`$*CWD`'s pre-capture snapshot.** `init_io_environment`'s comment records
  that `$*CWD` must be a fresh `IO::Path` at every interpreter start because
  `box_captured_lexicals` can box it into a captured cell. A dynamic that is
  never captured is never boxed, so this constraint weakens rather than
  tightens — but it must be re-checked, not assumed.

## 5. What shipped now, and what did not

This ADR is the design record; #7557's PR ships only the two changes that need
no part of it, because they are pure bookkeeping with no semantic surface:

1. **The capture memo stops arming when it never pays off.** #7624's `wants_arm`
   asks "did the previous capture see the same tier addresses", and the
   allocator recycles addresses: a closure created in a fresh frame gets the same
   overlay address back once the previous frame's map is freed, so `wants_arm`
   says yes — and the entry can then never be hit, because arming holds an `Arc`
   on that very map and forces the next frame to allocate elsewhere. The memo
   armed on every other creation and hit on none, at 4.5% of the loop in §1.2.
   Arms replaced without a hit are now counted, and the memo backs off.
2. **`capture_bare_callees`' `__mutsu_in_eval` probe is pre-interned**, removing
   a thread-local string-keyed intern of a fixed literal per creation — the same
   shape as #7557 Part A's fixes.

Measured (callgrind, deterministic): `my $c = * + 1;` × 200000 **−19.8%**, the
sub-frame loop of §1.2 **−8.5%**, `word-count` −0.42%, and `bench-ctor` /
`bench-class` / `bench-fib` / `bench-grammar-parse` / `bench-yaml-parse` flat
(±0.07%).

Neither touches the kept set, so the O(kept env) capture this ADR is about is
unchanged: that is §2's work.

## 6. Acceptance

- A capture from a mainline scope holds no `*`-twigil key unless the program
  bound one; the empty-program capture goes from 23 entries to 3.
- The sub-frame loop of §1.2 loses the ~20 inserts, `Value` clones and matching
  drops per creation, and the per-call merge loses the ~20
  `entry_or_insert_sym_with` chain walks that currently resolve to "already
  present".
- `t/start-dynamic-var-indir.t`, `t/start-inherits-dynamic-out.t`,
  `roast/S32-io/indir.t` and the `$DYNAMIC::`/`CALLER::` tests stay green.
- `make test` and `make roast` stay green.

## 7. Alternatives rejected

- **Trust free-variable analysis and drop any dynamic the closure body does not
  name.** This is the framing #7557 proposed and it is unsound: a closure that
  calls `say` reaches `$*OUT` through a callee, not through a name the free-var
  pass can see. §1.3's argument does not depend on analysing the body at all.
- **Drop a dynamic whose captured value is identical to the one the interpreter
  seeded.** Sound-looking (a user shadow has a different value, so it is kept)
  and cheap — but it makes the capture's correctness depend on the root env
  still being in the chain at *call* time, which is an invariant nothing states
  and nothing enforces. A per-interpreter base tier makes the same guarantee
  structural.
- **Unify the `$*X` and `*X` spellings** to halve the family. Worth doing on its
  own merits, but it is a change to every dynamic-variable read site and it
  leaves the remaining ~10 entries being copied per creation.
- **Do nothing.** Defensible: after #7624, #7682 and §5 the per-creation cost is
  roughly half what #7557 opened with. But the cost is O(kept env) on every
  capture that misses a one-entry memo, and the entries it is spent on are
  provably never read (§1.3).
