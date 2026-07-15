# ADR-0001: GC adoption — mechanism selection and phasing

- **Status**: Accepted (2026-06-27 — level 1 adopted. The collection-trigger mechanism §4.2 and the A' scope §4.3 remain open)
- **Date**: 2026-06-27
- **Deciders**: tokuhirom, Claude
- **Related**: [ANALYSIS.md](../../ANALYSIS.md) §2.1 / §1.3 / §5, [PLAN.md](../../PLAN.md) §G(perf) / §I(Track C)

> This file records the design judgment around "when, and with which mechanism, to introduce GC into mutsu".
> Points where the final decision is not yet fixed are separated into the "Open questions" section, distinct from the agreed direction.
> This ADR is the starting point for any future work that touches this decision; if the judgment changes, supersede it with a new ADR.

---

## 1. Context

### 1.1 Current state

- mutsu's memory management is **`Arc` reference counting only**. There is no tracing GC and no cycle collector.
  → **Cyclic references leak** (`value/mod.rs`). `Weak` is limited to local countermeasures such as `CONSUMED_SEQS` / `WeakSub` /
  `ptr_keyed.rs`, and is not systematic.
- Cycles are trivially expressible in ordinary Raku: env self-capture in recursive closures, mutually-referencing objects (parent-child, graphs,
  doubly-linked lists), self-bindings of the `%h<k> := %h` kind (the MAX_DESCENT in `element-element-bind-plan.md` is
  "infinite-loop prevention" — it **does not reclaim memory**), and Promise/Supply channel mutual references.
  → **Real harm exists.**

### 1.2 Relationship to project policy

- Definition of gain (CLAUDE.md): gain = the correct architecture (Raku compatibility + speed + no flakiness).
- User policy: **first catch up to raku and reach sufficient speed → then build an implementation with clear advantages on top of that**.
- Once that phase is reached, **an interpreter without GC will be regarded as "defective" and nobody will use it**.
  → GC is not a question of "whether" but **table stakes (the minimum bar for product quality)**. The open questions are *when and with which mechanism*.

### 1.3 Technical constraints binding the decision

- **mutsu already executes multi-threaded**: `start{}` / `Promise` / `hyper` / `race`.
  `Value` is `Arc`-based and assumes **Send + Sync**. The substance of cross-thread sharing is `clone_for_thread` →
  `shared_vars: RwLock<HashMap>` (`runtime/mod.rs`, ANALYSIS §2.1).
- **mutsu sits on top of Rust's ownership model**: `Value` is a Rust enum and lives **raw** all over Rust stacks, local variables,
  temporaries, and `Vec<Value>`s. The VM does not know the location of every Value.

---

## 2. Reference: the GC of the reference implementation (MoarVM)

Rakudo → NQP → **MoarVM**. The GC belongs to MoarVM, and its structure is:

- **Generational**:
  - **Nursery (young generation) = semi-space copying**. Each thread owns its own nursery, and **alloc = bump
    pointer (just advancing a pointer)** — extremely fast. At collection time, objects are copied between tospace/fromspace.
  - **Gen2 (old generation)**: objects that survive a nursery collection are promoted (tenured). Size-segregated
    free-list pools plus large objects; reclamation happens mark-sweep-style in a full collection.
- **Moving (copying)**: nursery objects change addresses → repointed via forwarding pointers.
- **Parallel / stop-the-world**: at collection time all threads are stopped at a safepoint, and the collection work is divided across multiple threads.
  Inter-generational references are recorded in a remembered set via a **write barrier**.
- **No refcounting whatsoever.**

**Key implication**: a tracing GC has no atomic inc/dec per mutation, and alloc is a blazing-fast bump pointer.
In other words, from the standpoint of "single-thread speed", **a tracing GC is faster than refcount/cycle-collector approaches**.

**Why MoarVM can do this**: because the VM owns the world completely. Every object carries an `MVMObject`
header, values are accessed via frames managed under `MVMThreadContext`, and even C extensions explicitly register
roots with `MVMROOT`. → The GC knows the exact location of every root and every pointer, and can safely move objects.

**The fundamental difference from mutsu**: mutsu has raw Values scattered across Rust stacks, so the "full knowledge of all roots"
that a precise moving GC requires is impossible. Achieving it would require a **full redesign** in which Values are only ever
touched through handles (level 2, described below).

---

## 3. Decision (agreed direction)

> The following is the direction agreed in the 2026-06-27 discussion. **Forks still awaiting final approval are separated into §4.**

1. **GC is ultimately mandatory** (a product-quality requirement). The only open questions are timing and mechanism.

2. **Fix the phase ordering**:

   | Phase | Content | Corresponding items in current PLAN.md | Impact on single-thread speed |
   |---|---|---|---|
   | **A. Catch up** | Match raku on compatibility + speed | §F roast, §2 multi-dispatch, §H module, perf Lever 1/3 | — |
   | **A'. Groundwork** | Consolidate roots to make the GC implementation easier | §1.4 lexical scope, §1.3 upvalue index conversion | — |
   | **B. Value-representation rework + GC** | Layer 3a below (§3) | §2.1 Track B, GC (new) | Neutral (type filter keeps hot path at 0) |
   | **C. JIT** | Distinct advantage of its own | perf Lever 4 | Improvement |

   - **GC before JIT**: retrofitting object references inside JIT-generated native code into GC root scanning /
     safepoints is hard. If GC lands first, the JIT can emit GC-aware roots from day one.
   - **Start GC only after Phase A is done**: introducing GC before catching up merely yields a "slow but
     leak-free interpreter". For now, GC stays in PLAN.md's "future" section.

3. **Moving GC (MoarVM-style nursery copying) is rejected**: incompatible with Rust ownership + directly held Arcs.
   Full root knowledge is impossible, and moving addresses would completely break `arc_contents_mut` (raw-pointer writes).

4. **The primary candidate is a cycle collector on Arc (Bacon-Rajan trial-deletion / concurrent cycle collection)**:
   - Keeps `Arc`'s Send + Sync → does not break the existing cross-thread model (start/Promise/hyper/race, `shared_vars`).
     Bacon-Rajan was originally designed for concurrent environments.
   - No wholesale replacement of the Value representation → the blast radius is orders of magnitude smaller than tracing.
   - Targets exactly "the real harm of cyclic leaks" and nothing more. Directly tied to the gain definition (compatibility, correctness).

5. **Protect the hot path with a type filter**: introduce `Gc<T>` management **only for the container-kind variants that can form cycles**.
   - In scope (container-kind): `Array` / `Hash` / `Set` / `Bag` / `Mix` / `Pair` / `Instance` / `Sub`(closure) /
     `ContainerRef`.
   - Out of scope (scalar-kind, cannot form cycles): `Int` / `Num` / `Rat` / `Complex` / `Str` / `Bool` / `Nil`.
   - → **Numeric loops and string processing (fib etc.) register zero candidates**. mutsu's benchmark bottlenecks (numerics,
     method calls) are unaffected by GC. "Bacon-Rajan is slow" applies to the naïve case of applying it uniformly to all
     objects; filtering by type avoids it.

6. **Layer split (breakdown of the Value-representation rework)**:

   | Layer | Content | Relationship |
   |---|---|---|
   | **3a** | Track B (element cell-ification) + cycle collector = one-shot `Arc → Gc<T>` replacement of the container kinds | **Inseparable. Executed as one campaign** |
   | **3b** | NaN-boxing (Value 48→8B) | **Groundwork for the JIT** (fixed 8B, simple tags, fits in a register). After GC, before JIT. Order 3a→3b→4 |
   | **3c** | Biased reference counting (operations from the owning thread are non-atomic) | Independent perf work after GC. **Priority drops once the JIT natively compiles int loops** (refcounting disappears from the hot path) |

   - **Why 3a is inseparable**: Track B and GC both touch the same `Arc<ArrayData>` / `Arc<HashData>` family (ANALYSIS §2.1,
     79 sites). Doing them separately means touching those 79 sites twice, each round reworking the other. Furthermore, Track B's biggest
     landmine — "VM re-entry deadlock while holding an `arc_contents_mut` borrow" — and the cycle collector's "when to run
     collection (safepoint)" question are **the same re-entry-boundary problem**; designed together, they can be unified.
   - **Why 3b can be separated**: under the cycle-collector plan, Arc/Gc pointers remain, so NaN-boxing simply becomes
     "scalar = payload / container = Gc pointer tag". The tracing-GC mark-bit placement problem does not
     arise, so it is no longer mandatory to do simultaneously with GC.

7. **Verification means**: add counters such as `gc_candidate_pushes` / `gc_collections` to `MUTSU_VM_STATS`, and
   **guarantee by measurement that candidate pushes = 0 on the fib bench** (the counters are independent of optimization level and iterated with debug builds
   — matching the CLAUDE.md policy). Monitor "GC made the hot path slower" with numbers, not guesses.

8. **Adopt level 1 (cycle collector on Arc)** (decided 2026-06-27). The policy is "it suffices to solve cyclic
   references; performance is earned by the JIT". The chain is consistent: **demanding no performance from GC = keeping GC simple = making the JIT easier to land**.
   - **Level 1 is also the easier one to put a JIT on**: level 1 is **non-moving + refcount**. The three points that turn JIT+GC
     cooperation into hell (① stack-map generation at every safepoint ② forwarding under moving ③ write-barrier emission)
     essentially disappear. Being non-moving, pointers never move so no forwarding is needed; refcounting guarantees liveness, so root
     scanning needs no JIT stack maps in principle. **All the JIT does is "emit Arc inc/dec".**
   - **Level 2 has the highest JIT performance ceiling, but the JIT implementation itself is full-VM grade** (MoarVM/V8/JVM-class
     stack maps, forwarding, barriers). In terms of "ease of landing" it is, conversely, the hardest.
   - **It matches mutsu's bottlenecks**: numerics, fib, and method calls are the bottlenecks, and Int/Num/Bool are excluded from
     Gc by the type filter. Once the JIT compiles int loops to native ints, the inner loop becomes **fully native
     code with zero GC and zero refcounting**, so the hot path performs almost the same as level 2. The need to build a heavyweight GC to chase performance disappears.
   - **Level 2 is shelved as a long-term option**: only if the level-1 JIT hits the refcount-cost ceiling (object-heavy code)
     and that is shown **by measurement** to be the bottleneck, re-evaluate it with a new ADR.

---

## 4. Open questions (only 4.2 / 4.3 remain — 4.1 was decided in §3-8)

### 4.1 Level 1 or level 2 → **decided: level 1 (§3-8)**

On 2026-06-27, **level 1 (cycle collector on Arc) was adopted**. Rationale in §3-8 (especially the JIT angle:
non-moving + refcount avoids JIT stack maps/forwarding/barriers, and with the type filter the numeric hot path reaches
zero GC/refcount cost via JIT native compilation). Level 2 (full VM redesign for MoarVM-style precise moving tracing) is
shelved as a long-term option, to be re-evaluated with a new ADR only if the level-1 JIT hits the refcount-cost ceiling **in actual measurement**.

For reference (characteristics of the rejected level 2): a full redesign managing every Value via handles/heap, with no raw
Values on Rust stacks. Its performance ceiling is the highest (bump alloc, zero refcount cost), but it is effectively a reimplementation of the interpreter — on the order of years —
and throws away much of the safety benefit of writing in Rust.

### 4.2 How cycle collection is triggered

- Synchronous (triggered when the candidate buffer exceeds a threshold, trial-deletion scan on the spot)
- Asynchronous (concurrent collection on a background thread; single-threaded execution only does pushes)

→ To be decided during 3a design, now that 4.1 has settled on level 1.

### 4.3 Scope of A' (groundwork)

How much of §1.4 lexical-scope introduction and §1.3 upvalue index conversion counts as a "GC prerequisite".
If roots can be consolidated into "the frame's `Vec<Value>` + the upvalue array", root enumeration for the cycle collector becomes simple.
The deciding factor is that as long as the env HashMap remains on the root path, scanning stays complicated.

---

## 5. Consequences

- **Do not start on the GC proper until Phase A is complete.** GC stays in PLAN.md's "future" section.
- **Do not start Track B standalone ahead of time.** Change the premise to integrating it with GC (layer 3a). Add to ANALYSIS §2.1 / §7-6
  that "Track B is one with GC (one-shot Arc→Gc replacement)".
- **perf Lever 2 (NaN-boxing) is not mandatory simultaneously with GC** (layer 3b, fine after GC) — reflect this in PLAN.md §G.
- **Add biased refcounting as a new perf work item (layer 3c) to PLAN.md §G** (a box alongside NaN-boxing and JIT).
- When starting layer 3a, perform the `Arc → Gc` replacement of the container-kind variants and Track B element cell-ification as **one campaign**.
- The implementation details of layer 3a (cooperative STW / root enumeration / how to carve the first wave of async nodes) are
  managed in [gc-level1-detailed-design.md](../gc-level1-detailed-design.md).

- **Add agent-facing rules to CLAUDE.md**: the ADR convention (`docs/adr/`) and a summary of this ADR's GC/JIT
  roadmap (phase ordering, moving rejected, cycle collector, type filter, Track B integration, level-1/2 open).

> Note: this ADR is a record of direction; propagating the above into PLAN.md / ANALYSIS.md (document reorganization) is
> carried out as separate work (this session chose "settle the discussion first", so it is not yet reflected). The rule
> additions to CLAUDE.md were done in this PR.

---

## 6. Alternatives considered

| Option | alloc/mutate speed | Implementation feasibility for mutsu | Relationship to Rust ownership | Verdict |
|---|---|---|---|---|
| **MoarVM-style generational moving tracing** | Fastest (bump alloc, refcount cost 0) | ✕ Handle-ifying every Value = full VM redesign | Abandoned | Shelved long-term as level 2 |
| **Conservative non-moving mark-sweep (Boehm-style)** | Medium (no bump; Arc-coexistence problem) | △ Dual Arc/GC management, false positives, MT stack scanning are landmines; tends to end up harder than a cycle collector | Half abandoned | Leaning reject |
| **Existing GC libraries (gc-arena / rust-gc / broom)** | — | ✕ Essentially single-thread (`Gc<T>: !Send`), no cross-thread | — | Rejected (no MT support) |
| **Cycle collector on Arc (Bacon-Rajan)** | Refcount cost remains, but type filter keeps hot path at 0 | ○ Minimally invasive, reuses existing Arc | Preserved | **Primary candidate (level 1)** |

---

*This ADR records the design discussion of 2026-06-27. If the judgment changes, supersede it with a new ADR.*
