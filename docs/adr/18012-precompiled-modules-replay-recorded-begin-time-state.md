# ADR-18012: A precompiled module replays its recorded BEGIN-time state instead of re-running BEGIN blocks and traits

- **Status**: Accepted (maintainer decision 2026-11-21; proposed 2026-11-21)
- **Issue**: [#18012](https://github.com/tokuhirom/mutsu/issues/18012) (uniname-words 8.5x,
  MagickWand 11x, Readline 7x rakudo on a warm load). It also serves
  [#18003](https://github.com/tokuhirom/mutsu/issues/18003).
- **Resolves**: the divergence that [ADR-0134](0134-begin-time-prologue.md) §3 accepts ("module
  BEGINs run on every load ... until precompiled bytecode exists"). It also resolves the
  "BEGIN-time state" item that [ADR-11756](11756-compiled-bytecode-precompilation.md) §2 defers to
  "a separate ADR".
- **Builds on**:
  - [ADR-11756](11756-compiled-bytecode-precompilation.md): the compiled-section cache, its key
    and its privacy rules.
  - [ADR-12026](12026-precomp-hit-loads-from-facts-and-decodes-lazily.md): a hit is served from
    load facts, and registration stays executed code.

## 1. Context

Rakudo precompiles a module once. The precompiled compunit carries its **serialized BEGIN-time
state**: the values that BEGIN blocks and `constant`s left in lexicals, and the routines and
types that traits mixed into. A warm load deserializes that state and runs no BEGIN code at all.

mutsu caches compiled code (ADR-11756, ADR-12026) but not BEGIN-time state, so every load re-runs
the module's BEGIN prologue and every trait application. The measured cost, release build, both
implementations warm:

| load | mutsu warm | rakudo warm | rakudo cold | where mutsu's time goes |
|---|---:|---:|---:|---|
| uniname-words `t/01-basic.rakutest` | 7.95 s | 0.93 s | 11.4 s | 90%: a `BEGIN` that builds a table over every code point |
| `use MagickWand` | 3.85 s | 0.34 s | 15.0 s | 82%: 670 `is native` traits, each a parametric-role mixin |
| `use Readline` | 2.5 s | 0.40 s | 8.1 s | 280 `is native(LIBREADLINE)` traits, each re-running a `dir(:test(rx))` |

mutsu's warm load is faster than rakudo's *cold* one on all three, so no single operation is
slow: the work is repeated on every load. Making the work faster cannot close a 7-11x gap. The
close condition of #18012 (within 3x of rakudo warm) is reachable only by not doing the work on
a warm load.

There is also a compatibility gap, not only a speed one. Rakudo evaluates a module's BEGIN-time
environment reads (`%*ENV`, `dir`, `$*KERNEL`) once, at precompilation, and freezes the result.
mutsu re-reads them on every load, so a module whose BEGIN reads the filesystem can behave
differently on mutsu and rakudo.

## 2. Decision

### 2.1 Record, then replay

The **first** load of a module (a precompilation miss) runs exactly as today. While it runs, the
loader records a **BEGIN effect log**: what the unit's BEGIN-time phase changed. That phase is the
ADR-0134 prologue, `constant` initializers, `use`-time imports and trait applications.

The log is stored in the module's ADR-11756 cache entry, next to the compiled section. A
**hit** replays the log in place of running the prologue and the traits. Replay is data
application, not code execution: it binds recorded values and attaches recorded mixins. It
calls no Raku code.

### 2.2 What the log records

Three record kinds, each added in its own slice (§4):

1. **Static lexical values.** These are the final value of each unit-level lexical that a BEGIN
   or a `constant` wrote, in the frame shape ADR-0134 §2.1 defines: a Scalar gets a fresh
   container holding the value, and an `@`/`%` lexical binds the same object. The value must be
   in the serializable set: `Int`, `Num`, `Rat`, `Str`, `Bool`, `Nil`, enums, type objects
   (recorded by qualified name), `Buf`/`Blob`, and `List`/`Array`/`Hash`/`Map`/`Pair` of those.
2. **Trait results on declarations.** A trait that mixes a role into a routine or attribute is
   recorded as *"declaration D does role R with arguments A"*. D is the declaration's stable site
   id, which ADR-11756 already hashes. A is a list of serializable values and references to other
   declarations by site id; for example, `NativeCall::Native[$r, $lib]` names `$r` by its own
   site id. Replay composes the same mixin the trait composed.
3. **Registry deltas** that a BEGIN or trait made outside the unit's own declarations: an
   `EXPORTHOW` declarator, a metamodel `add_method`, or a stash entry. Each is one typed record.
   ADR-12026 keeps the unit's own registration as executed code, and this ADR does not change
   that.

### 2.3 Anything else falls back to today's behaviour

If any BEGIN-time effect produces something the log cannot represent, the entry is marked
**replay-unsafe**. Examples: a closure stored in a lexical, an instance with arbitrary
attributes, an open handle, a value derived from a thread or Promise. A replay-unsafe entry
loads exactly as today, re-running the prologue and the traits. The fallback is the existing,
tested path, so an incomplete log can cost speed, never correctness. This is the "sound
mechanism" preference in AGENTS.md: replay happens only when the log is complete by
construction.

### 2.4 Invalidation

The log lives in the ADR-11756 entry and is invalidated with it: by the source hash, the
dependency entries' hashes and the mutsu build id. Like rakudo, a BEGIN that reads the
environment sees it **at precompilation**, and its result is frozen until the entry is
invalidated. This matches rakudo, and §1 counts it as a compatibility gain. A `no precompilation`
module never gets a log.

### 2.5 Trust boundary

The log is read from the cache directory that ADR-11756 already trusts for compiled sections,
under the same privacy and `O_NOFOLLOW` rules, so this adds no new trusted input. Replay
executes no Raku code and never `EVAL`s a recorded string. A malformed or truncated log is
rejected as a cache miss, never trusted partially. Decoding is bounded by the entry size and
raises no panic (docs/security.md: allocations sized by untrusted input need a bound).

## 3. Consequences

- **Expected gain.** uniname-words, MagickWand and Readline drop the 80-90% that is BEGIN and
  trait work on a warm load. That is the only route to #18012's close condition.
- **BEGIN-time side effects run once.** Output from a module BEGIN (`say` in a BEGIN) appears
  at precompilation only, as on rakudo. A test that relied on mutsu's per-load re-run is wrong
  by rakudo's standard and is updated.
- **Each record kind has two implementations: the trait or BEGIN that produces the effect, and
  the replay that re-applies it.** A parity test pins that they agree. Every slice runs each
  affected module's test suite cold (record) and warm (replay) and compares the outputs.
- **The cache entry grows** by the size of the log. uniname-words' table is the largest known
  case: one hash of names over the code-point range.

## 4. Implementation plan

| slice | records | first target |
|---|---|---|
| S1 | static lexical values (§2.2-1), the replay-unsafe marker, the cold/warm parity harness | uniname-words `t/01-basic` within 3x rakudo warm |
| S2 | trait mixins with arguments (§2.2-2) | `use MagickWand` and `use Readline` within 3x rakudo warm |
| S3 | registry deltas (§2.2-3) | modules whose BEGIN reaches the metamodel; target set from the ecosystem ledger |

## 5. Alternatives considered

- **Keep re-running the work and make each piece faster** (cheaper `dir`, cheaper mixin
  composition). Rejected as the plan, kept as slices: mutsu already beats rakudo's cold load,
  so the work is bounded by its volume, not its speed. The bounded slices #18012 names stay
  worth taking.
- **Snapshot the interpreter heap after the BEGIN phase.** Rejected: it serializes pointers,
  thread state and everything reachable, it cannot be bounded or validated, and it would make
  the cache a code-execution boundary.
- **Serialize values as `.raku` source and `EVAL` them on load.** Rejected: it runs code from
  the cache (a trust-boundary change), it is lossy for type objects and containers, and it is
  slower than direct decoding.

## 6. Open questions

- Whether a role argument that is itself a parametric type (`Native[$r, CArray[int32]]`) needs a
  type-expression record or can be named by its canonical name. S2 decides this from the
  MagickWand corpus.
- Whether S3 should instead extend ADR-12026's load facts. This is decided at S3, once S1-S2
  have shown how large the deltas are.

## Amendment 2026-11-22: S1 replays per `BEGIN` expression, checked against its inputs (Proposed)

- **Status**: Proposed (S1 implementation, #18012)

§2.3 marks a whole entry replay-unsafe when any effect cannot be logged. S1 records at a finer
grain: each value-position `BEGIN` of a module mainline (`BeginOnceExpr`) is its own record. A
body that cannot be proven replay-safe keeps running on every load. The other bodies of the
same mainline still replay. The prologue-lifted `BEGIN`s of ADR-0134 are outside S1 and keep
running, as today.

A body is recorded only when all four of these hold:

1. **Static scan.** Every opcode is on an allow-list. Methods and `nqp::` ops are admitted
   only by name, and only when their built-in form computes a value. The scan admits no
   declaration, no closure, no read of a global, package or dynamic variable, and no output.
2. **Code watch.** No other compiled code runs while the body runs. Every dispatch loop and
   the JIT entry build their polls with `DispatchPolls::enter(code)`, which is the only
   constructor. While a body is recorded, that constructor reports the code it is given.
   TRIR routine entry reports too.
3. **Faithful values.** The value and the changed unit lexicals decode back to exactly what
   they are. They share no container with each other or with any lexical that outlives the
   body.
4. **Inputs.** The log keeps the encoding of every unit lexical the body reads or writes, as it
   was before the body ran. A replay compares these first. If they differ, the body runs
   instead. This happens when an earlier `BEGIN` that is not replayed sets one of those
   lexicals differently on another load.

Conditions 1 and 2 leave the body no input other than its code, its constants and the lexicals
of condition 4. Its result is therefore a function of those, and the per-expression grain
stays complete by construction, as §2.3 requires.

One recording run cannot store a complete log: a mainline that reaches a `BEGIN` which already
ran during the module's own compile. That run stores no log at all. The next load runs the
mainline without compiling anything, so it records the whole log.

`MUTSU_BEGIN_REPLAY=0` turns S1 off.
