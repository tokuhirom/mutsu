# mutsu - Roadmap

Goal: Build a practical Raku (Perl 6) runtime in Rust that is faster than MoarVM.

Current status (2026-03-13): roast 675/1463 whitelisted (46%), 662 passing in latest run.
Hybrid architecture: bytecode VM with interpreter bridge fully eliminated.

---

## Completed

Phase 1-4 (language core, OOP, regex/grammar basics, advanced features) are done.
See `docs/vm-compilation-history.md` for VM compilation history.

- All basic types, full operator set, control flow, OOP (class/role/enum/subset), basic regex/grammar
- Phasers (BEGIN, END, ENTER, LEAVE, FIRST, NEXT, LAST), concurrency scaffolding
- Bytecode VM with native dispatch, interpreter bridge fully removed
- Signature system (where clauses, destructuring, capture params, is copy/rw, sub-signatures, introspection, callframe/caller/wrap)
- Container model (Proxy, auto-vivification, typed containers, temp/let, .VAR)
- Type system (allomorphs, type objects, coercion types, subset where, typed assignment)
- Module system (export, import, require, unit, EXPORT convention, CompUnit, precompilation)
- Grammar/regex (action classes, inheritance, protoregex, LTM, closures in regex, named captures)
- Class system (re-dispatch, anonymous classes, parameterized roles, trusts, augment, MOP, FALLBACK, handles, is required/built)
- Concurrency (non-blocking await, scheduler, Lock/Lock::Async, atomics, Supply combinators, Supplier::Preserving, hyper/race)
- Memory: Arc-wrapped heap types, WeakSub cycle-breaking, scoped call frames

---

## High-impact blockers (blocker analysis 2026-03-13)

Remaining 545 failing tests cluster around a few systemic gaps.
Fixing these unlocks tests across many synopses at once.

### B1. Phasers: INIT, CHECK, and phaser ordering (~20+ tests)

14 S04-phasers tests fail. INIT/CHECK are almost completely broken.
Phasers also appear in S06, S12, S04-statements tests as prerequisites.

- [ ] INIT phaser (run at beginning of runtime, after all CHECK/BEGIN)
- [ ] CHECK phaser (run at end of compile time, reverse order)
- [ ] Correct phaser ordering (BEGIN forward, CHECK reverse, INIT forward)
- [ ] Phaser rvalue semantics (`my $x = BEGIN { 42 }`)
- [ ] KEEP/UNDO phasers
- [ ] `once` phaser

### B2. Type constraints and signature strictness (~25+ tests)

S06-signature (15 fail), S02-types (32 fail), S09-typed-arrays (13 fail).

- [ ] Smiley parameters (`:D` / `:U` type constraints)
- [ ] Type capture (`::T` in signatures)
- [ ] Signature type-checking enforcement (reject wrong-type args with proper X::TypeCheck)
- [ ] Native int/uint overflow and bounds checking
- [ ] `is copy` trait on non-scalar parameters
- [ ] Multiple signatures on a single sub
- [ ] Proper `return` as control exception (currently uses Rust panic-like flow)

### B3. Missing methods on core types (~30+ tests)

27 tests hit `Unknown method` and stop mid-execution.
These are individually small but collectively a large blocker.

- [ ] `.can` (introspection)
- [ ] `.does` (role checking)
- [ ] `.resume` (exception resumption)
- [ ] `.pending` (Promise)
- [ ] `.dynamic` (variable introspection)
- [ ] `.decode` (Buf)
- [ ] `.push` on shaped/multi-dimensional arrays
- [ ] `.invert` (Bag/Set/Hash)
- [ ] `.count-only` (iterator protocol)
- [ ] `.AT-POS` / `.EXISTS-POS` (custom subscript protocol)

### B4. OOP completeness (~55 tests across S12, S14)

S12 (55 fail), S14 (16 fail). Many subtests pass but runtime errors stop execution.

- [ ] Class-level `is rw` (all attributes writable)
- [ ] Namespaced class construction (`A::B.new`)
- [ ] Attribute introspection completeness (`.^attributes` listing)
- [ ] `augment class` improvements (augmenting with new attributes)
- [ ] Role conflict detection and error reporting
- [ ] Parameterized role mixin
- [ ] Destruction/GC hooks (DESTROY)

### B5. Supply/react/whenever completeness (~27 tests)

S17-supply has 27 failures. Basic supply works but many combinators and edge cases fail.

- [ ] Supply error propagation and done semantics
- [ ] Supply backpressure
- [ ] `supply`/`react` block scoping issues
- [ ] Tap management (close, drain)

### B6. IO and process (~32 tests)

S32-io has 32 failures. File/directory operations and process management.

- [ ] IO::Path methods completeness (`.resolve`, `.cleanup`, `.parts`)
- [ ] IO::Handle read modes (binary, encodings)
- [ ] Proc and Proc::Async completeness
- [ ] File test operators (`-e`, `-f`, `-d` etc.)

### B7. Regex/grammar remaining gaps (~41 tests)

S05 has 41 failures. Core regex works but advanced features missing.

- [ ] Match object `.caps` / `.chunks`
- [ ] Lookbehind assertions (`<!after>`)
- [ ] `~` tilde goal matching in regex
- [ ] Grammar rule arguments
- [ ] Regex interpolation of arrays/variables
- [ ] Remaining `:Perl5` modifier edge cases

---

## Low-hanging fruit (quick wins)

### Near-passing tests (1 subtest away)

| Test | Status | Blocker |
|------|--------|---------|
| S04-phasers/begin.t | 12/13 | BEGIN before parse error |
| S04-statements/redo.t | 11/12 | edge case |
| S03-junctions/misc.t | 108/155 | 1 failing subtest |
| S05-capture/named.t | 10/11 | 1 failing subtest |
| S02-literals/fmt-interpolation.t | 10/11 | 1 failing subtest |
| S03-operators/orelse.t | 11/16 | 1 failing subtest |
| S06-signature/unpack-array.t | 10/15 | 1 failing subtest |

### Single-fix unlocks (0 failing subtests, runtime error stops execution)

| Test | Status | Fix needed |
|------|--------|------------|
| S03-operators/set_proper_subset.t | 1160/1742 | Add `⊄` operator |
| S03-operators/buf.t | 174/194 | X::Assignment::RO edge case |
| S03-operators/relational.t | 60/179 | Numeric(Sub:D) coercion |
| S03-junctions/misc.t | 108/155 | junction autothread in smartmatch |
| S06-multi/redispatch.t | 10/14 | `samewith` function |
| S06-multi/positional-vs-named.t | 28/31 | proto sub matching |
| S06-traits/native-is-rw.t | 30/48 | method dispatch with native params |
| S11-modules/export.t | 38/59 | missing method `.b` |
| S02-types/mixed_multi_dimensional.t | 43/80 | `.push` on shaped arrays |

---

## Roast per-synopsis status (2026-03-13)

| Synopsis | Domain              | Pass | Fail | Total | Rate |
|----------|---------------------|------|------|-------|------|
| S02      | Literals, types     |   69 |   58 |   127 |  54% |
| S03      | Operators           |   74 |   41 |   115 |  64% |
| S04      | Control, phasers    |   32 |   39 |    71 |  45% |
| S05      | Regex, grammar      |   52 |   41 |    93 |  56% |
| S06      | Subs, signatures    |   49 |   36 |    85 |  58% |
| S07      | Iterators           |    4 |    2 |     6 |  67% |
| S09      | Arrays, subscripts  |    2 |   13 |    15 |  13% |
| S10      | Packages            |    3 |    6 |     9 |  33% |
| S11      | Modules             |   12 |   10 |    22 |  55% |
| S12      | Classes, roles      |   39 |   56 |    95 |  41% |
| S14      | Roles               |    7 |   16 |    23 |  30% |
| S15      | Unicode/NFG         |   63 |   16 |    79 |  80% |
| S16      | IO                  |   19 |   18 |    37 |  51% |
| S17      | Concurrency         |   49 |   50 |    99 |  49% |
| S19      | CLI                 |    4 |    4 |     8 |  50% |
| S24      | Testing             |   12 |    5 |    17 |  71% |
| S26      | Pod                 |    9 |   18 |    27 |  33% |
| S29      | Functions           |    8 |    6 |    14 |  57% |
| S32      | Built-in types      |  131 |  127 |   258 |  51% |

---

## Performance roadmap

### P1. Memory — remaining

- [ ] Cycle collector (for circular object references; trial deletion or epoch-based on top of Arc)

### P2. Optimization pipeline (future)

- [ ] Constant folding
- [ ] Dead code elimination
- [ ] Inlining (small subs)
- [ ] Native int/num (avoid boxing for hot paths)
- [ ] String rope / CoW
- [ ] Type inference (speculative optimization)
- [ ] Escape analysis
- [ ] Register-based VM or native code generation

---

## Practicality (future)

- [ ] REPL
- [ ] Improved error messages (with source location, suggestions)
- [ ] Debugger
- [ ] `zef` package manager compatibility
- [ ] Native binary output

---

## Design principles

1. **Correctness first, performance second** — pass roast, then optimize
2. **Top-down feature implementation** — identify high-impact blockers, implement systematically
3. **Learn from MoarVM** while leveraging Rust's ownership model
4. **Prioritize startup speed** (a weakness of MoarVM)
5. **No stubs or hacks** — every feature must be a genuine, general-purpose implementation
