# mutsu - Roadmap

Goal: Build a practical Raku (Perl 6) runtime in Rust that is faster than MoarVM.

Current status (2026-03): roast 580/1463 passing (39.6%). Hybrid bytecode VM + tree-walker.

---

## Completed milestones

Phase 1-4 (language core, OOP, regex/grammar basics, advanced features) are largely done.
See `docs/vm-compilation-history.md` for detailed VM compilation progress log.

Key completed items:
- All basic types (Int, Num, Str, Bool, Rat, Complex, Array, Hash, Set/Bag/Mix, Enum, Junction, Range, Pair)
- Full operator set (arithmetic, string, comparison, logical, bitwise, meta, hyper, reduction)
- Control flow (if/unless/while/for/loop/given/when/gather/take/try/CATCH)
- OOP (class, role, inheritance with C3 MRO, multi method, BUILD/TWEAK, enum, subset)
- Basic regex/grammar (rx/m/s, character classes, quantifiers, anchors, named captures, token/rule/grammar, proto token, make/made)
- Phasers (BEGIN, END, ENTER, LEAVE, FIRST, NEXT, LAST)
- Concurrency scaffolding (Promise, Supply, Channel, react/whenever, Proc::Async)
- Bytecode VM with native dispatch for ~50 binary ops, ~40 functions, ~50 methods
- Scalar container model
- Symbol table

---

## Architectural milestones (roast grinding alone won't surface these)

These are systemic improvements that unlock large batches of roast tests at once.
Priority order reflects dependency and impact.

### A1. Signature/parameter system overhaul (S06: 2/94 passing)

The single largest blocker. Many S06 tests require features beyond basic positional/named params.

- [x] `where` clauses on parameters (`Int $x where * > 0`)
- [x] Destructuring signatures (`sub foo([$a, $b]) { }`, `@a [$x, *@rest]`)
- [x] Capture parameters (`|c`)
- [x] `is copy` / `is rw` parameter traits
- [x] Sub-signatures (`sub foo(Int :x($val)) { }`)
- [x] Proper `Signature` / `Parameter` introspection objects
- [x] `callframe` / `caller` / `callwith` / `nextsame` / `nextwith`
- [x] `wrap` / `unwrap`
- [ ] Proper `return` as control exception (currently uses Rust panic-like flow)
- [ ] Tail call optimization (deep recursion crashes)

### A2. Container model completion

Scalar containers are done. Array/Hash container semantics need work.

- [x] `Proxy` container (for `is rw` accessor return values)
- [x] Array auto-vivification (`@a[5] = 42` on empty array)
- [x] Hash auto-vivification (`%h<a><b> = 1`)
- [x] `temp` / `let` variable save/restore (dynamic scope)
- [x] Typed containers (`my Int @a`, `my Str %h`)
- [x] Container `.VAR` introspection

### A3. Type system deepening (S02: 27/146 passing)

- [x] Allomorphic types (`IntStr`, `NumStr`, `RatStr`, `ComplexStr`)
- [x] Type objects vs instances (proper undefined-ness: `Int` is a type object)
- [x] `where` clauses on `subset` (partially done)
- [x] Coercion types (`Int(Str)` as type constraint, not just explicit coercion)
- [x] `is` trait on variables (`my $x is default(42)`)
- [x] Typed variable assignment enforcement at runtime

### A4. Module/package system (S10: 0/9, S11: 3/22 passing)

- [x] `export` / `is export` trait with tag support
- [x] `import` with selective import (`use Foo :bar`)
- [x] `require` at runtime (dynamic module loading)
- [x] `unit module` / `unit class` / `unit role`
- [x] Proper `EXPORT` sub convention (`EXPORT::TAG::symbol` namespace population)
- [x] `CompUnit` / repository API — `CompUnit::DependencySpecification`, `CompUnit::Repository` role, `$*REPO`, `$*REPO.need()` returning CompUnit with `.precompiled`
  - Remaining: `.handle.globalish-package`, dependency-aware invalidation
- [x] Precompilation — serde-based AST cache, mtime + version invalidation, `--no-precomp` flag, `no precompilation` pragma
  - Remaining: dependency-aware invalidation (recompile when transitive deps change)

### A5. Grammar/regex completion (S05: 10/98 passing)

- [x] Action classes (method dispatch on grammar rule match)
- [x] Grammar inheritance (`is Grammar`)
- [x] Full protoregex with longest-token matching
- [ ] Regex modifiers (`:g`, `:i`, `:ii`, `:s`, `:ss`, `:ratchet`, `:overlap`, `:exhaustive`)
- [x] Interpolating closures in regex (`/ { code } /`)
- [x] `$/` proper binding after match
- [ ] Match object full API (`.caps`, `.chunks` missing; positional captures done)
- [x] Named capture alias (`$<name>=<rule>`)
- [ ] Lookbehind (`<!before>`, `<!after>`)

### A6. Class system completion (S12: 5/101 passing)

- [x] `nextsame` / `nextwith` / `callsame` / `callwith` re-dispatch
- [x] Anonymous classes (`class { }`)
- [ ] Parameterized roles (`role Foo[Type] { }`)
- [ ] `trusts` declarator
- [ ] `augment class` (monkey-patching)
- [ ] Meta-object protocol (`.^methods`, `.^attributes`, `.^mro` etc.)
- [ ] `FALLBACK` method
- [ ] Full `handles` delegation (with rename, exclude)
- [ ] Attribute `is required` / `is built` traits

### A7. Concurrency overhaul (S17: 5/99 passing)

Current implementation uses blocking OS threads. Raku's concurrency model needs:

- [ ] Non-blocking `await` (green threads or async runtime integration)
- [ ] Scheduler (`Promise.in`, `Promise.at`, `Supply.interval`)
- [ ] `Lock` / `Lock::Async`
- [ ] Atomic operations (`cas`, `⚛++`)
- [ ] `Supply` combinators (`.batch`, `.grep`, `.map`, `.zip`, `.merge`, `.throttle`)
- [ ] `Supplier` / `Supplier::Preserving`
- [ ] `hyper` / `race` (parallel iteration)

---

## Performance roadmap

### P1. Memory management / GC

Design doc: `docs/gc.md`

#### Phase 1: Arc-wrap heap types (cheap clone via refcount bump)

- [x] Phase 1a: Env (CoW `Arc<HashMap>` wrapper — `src/env.rs`)
- [x] Array (`Arc<Vec<Value>>`)
- [x] Hash (`Arc<HashMap<String, Value>>`)
- [x] Sub (`Arc<SubData>`)
- [x] Instance attributes (`Arc<HashMap<String, Value>>`)
- [x] Set / Bag / Mix (`Arc<HashSet>` / `Arc<HashMap>`)
- [x] Seq / Slip / Junction values (`Arc<Vec<Value>>`)
- [x] Str (`Arc<String>`) — PR #748
- [x] BigInt (`Arc<NumBigInt>`)
- [x] GenericRange (`Arc<Value>`)
- [x] Mixin (`Arc<Value>, Arc<HashMap>`)
- [x] Regex / RegexWithAdverbs (pattern `Arc<String>`)

#### Phase 2: WeakSub for cycle-breaking

- [x] `WeakSub(Weak<SubData>)` for `&?BLOCK` self-references

#### Phase 3: Remaining memory issues

- [x] Scoped call frames — locals authoritative for simple vars with lazy env sync; `SetLocal` fast path skips env writes, `locals_dirty` flag triggers deferred flush before closure captures and interpreter bridge calls (#762)
- [ ] Cycle collector — for user-created circular object references (e.g. two instances pointing to each other); trial deletion or epoch-based approach on top of Arc

### P2. Interpreter bridge elimination

Remove remaining tree-walker fallbacks in the VM:
- [x] Closure compilation infrastructure (Lambda, AnonSub, AnonSubParams, BlockClosure bodies compiled to bytecode and stored in `SubData.compiled_code`)
- [x] Closure execution fast path (`call_compiled_closure` enabled for VM-internal closure calls; leave targeting fixed, `.assuming()` support added)
- [x] Class/role declaration compilation
- [x] Full method dispatch compilation
- [x] Remove `InterpretExpr` / `InterpretStmt` opcodes entirely

### P3. Optimization pipeline

- [ ] Constant folding
- [ ] Dead code elimination
- [ ] Inlining (small subs)
- [ ] Native int/num (avoid boxing for hot paths)
- [ ] String rope / CoW (reduce clone overhead)
- [ ] Type inference (speculative optimization)
- [ ] Escape analysis
- [ ] Register-based VM or native code generation

---

## Practicality

- [ ] REPL
- [ ] Improved error messages (with source location, suggestions)
- [ ] Debugger
- [ ] `zef` package manager compatibility
- [ ] Native binary output

---

## Ongoing: Roast test coverage

The primary day-to-day work. Use `pick-next-roast.sh` to select tests.
Current per-synopsis status (pass/total with known failures):

| Synopsis | Domain                    | Pass | Fail | Notes |
|----------|---------------------------|------|------|-------|
| S02      | Literals, types           |   27 |  119 | Allomorphs, type objects |
| S03      | Operators                 |   16 |  109 | Operator overloading, edge cases |
| S04      | Control, phasers          |    9 |   68 | `state`, full CATCH, phasers |
| S05      | Regex, grammar            |   10 |   88 | Action classes, modifiers |
| S06      | Subs, signatures          |    2 |   92 | Biggest gap — see A1 |
| S09      | Arrays, subscripts        |    0 |   22 | Subscript adverbs |
| S10      | Packages                  |    0 |    9 | See A4 |
| S12      | Classes, roles            |    5 |   96 | See A6 |
| S15      | Unicode/NFG               |   54 |   27 | Strongest area |
| S17      | Concurrency               |    5 |   94 | See A7 |
| S32      | Built-in types            |   16 |  261 | Largest absolute gap |

---

## Design principles

1. **Correctness first, performance second** — pass roast, then optimize
2. **Gradual migration** from tree-walking interpreter to full bytecode VM
3. **Learn from MoarVM** while leveraging Rust's ownership model
4. **Prioritize startup speed** (a weakness of MoarVM)
5. **No stubs or hacks** — every feature must be a genuine, general-purpose implementation
