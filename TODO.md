# mutsu - Roadmap

Goal: Build a practical Raku (Perl 6) runtime in Rust that is faster than MoarVM.

Current status (2026-03-15): roast 695/1296 passing (54%), 704 whitelisted.
Hybrid architecture: bytecode VM with interpreter bridge fully eliminated.

---

## Completed

Phase 1-4 (language core, OOP, regex/grammar basics, advanced features) are done.
See `docs/vm-compilation-history.md` for VM compilation history.

- All basic types, full operator set, control flow, OOP (class/role/enum/subset), basic regex/grammar
- Phasers (BEGIN, END, ENTER, LEAVE, FIRST, NEXT, LAST, INIT, CHECK, KEEP, UNDO, once), concurrency scaffolding
- Bytecode VM with native dispatch, interpreter bridge fully removed
- Signature system (where clauses, destructuring, capture params, is copy/rw, sub-signatures, introspection, callframe/caller/wrap, samewith)
- Container model (Proxy, auto-vivification, typed containers, temp/let, .VAR)
- Type system (allomorphs, type objects, coercion types, subset where, typed assignment, smiley :D/:U, ::T capture, Bool⊂Int hierarchy)
- Module system (export, import, require, unit, EXPORT convention, CompUnit, precompilation)
- Grammar/regex (action classes, inheritance, protoregex, LTM, closures in regex, named captures, rule arguments, tilde goal matching, regex array interpolation, <.ws> between word chars)
- Class system (re-dispatch, anonymous classes, parameterized roles, trusts, augment, MOP, FALLBACK, handles, is required/built, class-level is rw, DESTROY, role conflict detection, BUILD/TWEAK MRO ordering)
- Concurrency (non-blocking await, scheduler, Lock/Lock::Async, atomics, Supply combinators (16 methods), Supplier::Preserving, hyper/race)
- Introspection (.can, .does, .resume, .dynamic, .count-only, .^attributes, .^parents, .^roles, EXISTS-POS)
- NFC normalization (\c[], \x[], .chr, .uc, .lc, .flip, string repeat)
- Memory: Arc-wrapped heap types, WeakSub cycle-breaking, scoped call frames

---

## High-impact blockers (updated 2026-03-15)

Remaining ~515 failing tests. Many B1-B7 items have been resolved.

### B1. Phasers — mostly done

- [x] INIT phaser
- [x] CHECK phaser
- [x] Correct phaser ordering (BEGIN forward, CHECK reverse, INIT forward)
- [x] KEEP/UNDO phasers
- [x] `once` phaser
- [x] BEGIN as statement prefix (`my $x = BEGIN uc 'moin'`)
- [ ] Phaser rvalue caching (INIT/CHECK/BEGIN as rvalues in closures)
- [ ] ENTER phaser at top level (interpreter/VM scope boundary)
- [ ] PRE/POST phasers (contract programming)

### B2. Type constraints and signature strictness — partially done

- [x] Smiley parameters (`:D` / `:U` type constraints)
- [x] Type capture (`::T` in signatures)
- [x] Proper `return` as control exception
- [x] `samewith` function
- [x] `is copy` trait on array/hash parameters
- [x] Duplicate parameter detection
- [x] Invocant marker rejection in non-methods
- [x] Enhanced binding error messages
- [ ] Signature type-checking enforcement (reject wrong-type args with proper X::TypeCheck)
- [ ] Native int/uint overflow and bounds checking
- [ ] Multiple signatures on a single sub

### B3. Missing methods on core types — mostly done

- [x] `.can` (introspection)
- [x] `.does` (role checking)
- [x] `.resume` (exception resumption)
- [x] `.dynamic` (variable introspection)
- [x] `.decode` (Buf)
- [x] `.invert` (Bag/Set/Hash)
- [x] `.count-only` (iterator protocol)
- [x] `.AT-POS` / `.EXISTS-POS` (custom subscript protocol)
- [ ] `.pending` (Promise)
- [ ] `.push` on shaped/multi-dimensional arrays

### B4. OOP completeness — significant progress

- [x] Class-level `is rw` (all attributes writable)
- [x] Attribute introspection (`.^attributes`, `.^parents`, `.^roles`)
- [x] Role conflict detection and error reporting
- [x] DESTROY submethods
- [x] Nested class registrations
- [x] BUILD/TWEAK submethod dispatch order (MRO base-first)
- [ ] Namespaced class construction (`A::B.new`)
- [ ] `augment class` improvements (augmenting with new attributes)
- [ ] Parameterized role mixin

### B5. Supply/react/whenever — significant progress

- [x] Supply error propagation
- [x] 16 Supply combinator methods (sort, squish, head, flat, produce, batch, rotor, rotate, comb, words, snip, minmax, zip, zip-latest, start, wait)
- [x] Supply.from-list single-arg rule
- [ ] Supply backpressure
- [ ] `supply`/`react` block scoping issues
- [ ] Tap management (close, drain)

### B6. IO and process — improved

- [x] IO::Path methods (`.resolve`, `.cleanup`, `.parts`)
- [x] IO::Handle `.read()`, `.seek()`, `.slurp()`, `.tell()`
- [x] Proper `put` semantics (separate from `say`)
- [x] IO::Path.volume, .copy :createonly, directory open rejection
- [ ] IO::Handle read modes (binary, encodings)
- [ ] Proc and Proc::Async completeness
- [ ] File test operators (`-e`, `-f`, `-d` etc.)

### B7. Regex/grammar — improved

- [x] `~` tilde goal matching in regex
- [x] Grammar rule arguments
- [x] Regex interpolation of arrays/variables
- [x] `<.ws>` between word characters
- [x] Lookaround assertions
- [ ] Match object `.caps` / `.chunks`
- [ ] Lookbehind assertions (`<!after>`)
- [ ] Remaining `:Perl5` modifier edge cases

---

## Recent wins (2026-03-15 session)

| PR | Key change | Impact |
|----|-----------|--------|
| #1139 | UTF-8 panic fix in quote escapes | 0 panics |
| #1141 | WhateverCode prefix ops | relational.t +95 |
| #1144 | .^parents/.^roles/.^attributes | introspection unlocked |
| #1147 | NaN/≈ comparison fix | relational.t 179/179 |
| #1148 | put/IO fixes | +3 tests |
| #1149 | BUILD/TWEAK MRO order | TWEAK.t full pass |
| #1150 | Signature checks | +16 subtests |
| #1151 | regex ws fix | +7 tests |
| #1152 | 16 Supply combinators | +5 tests |
| #1153 | BEGIN prefix/leave fix | next.t full pass |
| #1155 | NFC normalization/Rat.base | +9 tests |
| #1157 | Bool⊂Int/Num.raku | +7 tests |
| #1158 | Signature introspection | +5 subtests |
| #1159 | eqv/map/Pod fix | +7 subtests |
| #1161 | Complex/Rat numeric edges | complex.t full pass |

---

## Roast per-synopsis status (2026-03-15)

| Synopsis | Domain              | Pass | Fail | Total | Rate |
|----------|---------------------|------|------|-------|------|
| S02      | Literals, types     |   76 |   51 |   127 |  60% |
| S03      | Operators           |   82 |   33 |   115 |  71% |
| S04      | Control, phasers    |   34 |   37 |    71 |  48% |
| S05      | Regex, grammar      |   53 |   40 |    93 |  57% |
| S06      | Subs, signatures    |   52 |   33 |    85 |  61% |
| S07      | Iterators           |    4 |    2 |     6 |  67% |
| S09      | Arrays, subscripts  |    2 |   13 |    15 |  13% |
| S10      | Packages            |    3 |    6 |     9 |  33% |
| S11      | Modules             |   12 |   10 |    22 |  55% |
| S12      | Classes, roles      |   42 |   53 |    95 |  44% |
| S14      | Roles               |    7 |   16 |    23 |  30% |
| S15      | Unicode/NFG         |   65 |   14 |    79 |  82% |
| S16      | IO                  |   22 |   15 |    37 |  59% |
| S17      | Concurrency         |   55 |   44 |    99 |  56% |
| S19      | CLI                 |    4 |    4 |     8 |  50% |
| S24      | Testing             |   12 |    5 |    17 |  71% |
| S26      | Pod                 |    9 |   18 |    27 |  33% |
| S29      | Functions           |    8 |    6 |    14 |  57% |
| S32      | Built-in types      |  140 |  118 |   258 |  54% |

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
