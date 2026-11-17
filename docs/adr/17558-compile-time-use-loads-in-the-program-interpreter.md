# ADR-17558: A `use` loads its module in the program's own interpreter when the parser reaches it

- **Status**: Accepted (decision: the maintainer, 2026-11-17: "match rakudo"; route B of the
  2026-11-12 decision recorded in ADR-17557). The open questions of §5 are answered by §7.
- Date: 2026-11-17
- Deciders: tokuhirom, Claude
- Issue: [#17558](https://github.com/tokuhirom/mutsu/issues/17558)
- Supersedes (as its slices land): [ADR-0124](0124-parse-time-export-probe-for-computed-export-stashes.md)
  (the parse-time export probe) and [ADR-17557](17557-parse-time-probe-for-begin-time-merges.md)
  (its BEGIN-call gate), and the probe half of [ADR-0026](0026-slang-activation-architecture.md)
  §2.1 (slang activation in a separate interpreter). It completes the step
  [ADR-0134](0134-begin-time-prologue.md) §2.4 left open for `use`.
- Related: [ADR-15762](15762-slangs-by-execution.md) (the slang host),
  [ADR-0087](0087-runtime-export-hook-parse-time-approximation.md) (the static export scan),
  [ADR-11756](11756-compiled-bytecode-precompilation.md) /
  [ADR-12026](12026-precomp-hit-loads-from-facts-and-decodes-lazily.md) (precompilation),
  [ADR-10779](10779-interpreter-subsystems-and-upward-call-traits.md) (upward-call traits),
  [docs/security.md](../security.md) ("Analysis never executes").

## 1. Context

rakudo compiles `use M` at BEGIN time: when the parser reaches the statement it
loads `M` (from precomp if it can, in which case `M`'s own BEGIN-time work ran
once, when `M` was compiled), imports `M`'s exports into the current lexical
scope, and only then parses the next statement. Everything `M` produced while
loading is part of the importer's grammar and scope from that point on: an
`EXPORTHOW` merged by a call, export names computed by `sub EXPORT` or by a loop
over `OUR::`, an operator whose precedence is computed, a slang.

mutsu parses a whole compilation unit first and loads modules when the unit
runs (since ADR-0134, in the unit's BEGIN prologue). The parser therefore has to
*predict* what a module will provide:

1. a static scan of the module source (`module_exports`, ADR-0087), cached on
   disk;
2. where the scan cannot see it, a **probe**: a fresh `Interpreter` on a fresh
   thread runs `use M` and reads back names. Three probes exist today:
   - slang activation (ADR-0026 §2.1, `runtime/slang_activation.rs`, kept alive
     as the slang host of ADR-15762);
   - computed export stashes (ADR-0124, `runtime/parse_time_exports.rs`);
   - BEGIN calls into a loaded module (ADR-17557, the same probe behind a wider
     gate).

This has three structural costs:

- **Every probed module runs twice** (probe, then the real load). Its output,
  file writes and other side effects happen twice. ADR-0124 §3 and ADR-17557 §3
  accept that as a trade-off.
- **Each new module shape needs a new scan rule and a new gate.** ADR-17557 is
  the third gate; the next shape (a computed operator precedence, an
  `EXPORT` sub that returns a role, ...) would be the fourth.
- **A probe only returns names.** The parser never sees the objects: a
  declarator's HOW, an operator's precedence record, a role an `EXPORT` made.
  Whatever the parser needs beyond "this name is a routine" is approximated.

`Never build an Interpreter to run code` (AGENTS.md) lists the parse-time probes
as one of the few allowed constructions; this ADR removes most of them.

## 2. Decision

### 2.1 The parser asks the program's interpreter to load the module

When the parser finishes a `use` statement (the whole statement, including its
`:ver`/`:auth` adverbs, import list and terminator), and the parse is a
*compiling* parse (§2.5), it calls a **compile-time loader** with the module
spec and the lexical scope the `use` is in. The loader is the program's own
`Interpreter`, the one that will run the unit:

- `main` already builds the `Interpreter` before it parses (`interpreter.run(&input)`
  parses on the same thread), so the loader is reachable without a new thread or
  a new interpreter. EVAL, `require` of a source file and a module's own compile
  reach it the same way, through the interpreter that is compiling them.
- The parser names the loader only through a trait in a leaf module (the
  `slang_link` pattern; ADR-10779's upward-call rule): the parser does not
  name `crate::runtime`.
- The load is the ordinary module load (repository chain, precomp, the
  module's own compile with the same loader for its own `use`s, its mainline).
  It is recorded in the interpreter's module registry as loaded, exactly as a
  run-time load records it today.

### 2.2 The parser reads the loaded unit, not a prediction

The loader answers with the **facts of the loaded unit** that the parse needs:
the exported routines, terms, types and operators of the tags the `use` imports
(with their real objects: an operator's precedence and associativity, a
declarator's HOW), the `EXPORTHOW` declarators, and the slang the module
activates. The parser registers them in the `use`'s lexical scope as it
registers scanned names today, so block scoping (ADR-17557 §2.4) is unchanged.

The static scan stays as the parse-time source only for the analysis paths
(§2.5). For a compiling parse it is no longer consulted for a module that was
loaded, and the probe gates (`dynamic_export_stash`, `begin_calls_loaded_routine`)
are deleted with the probes.

### 2.3 Run time imports, it does not load again

The unit's BEGIN prologue (ADR-0134) keeps its `use` effect at the same
position, but for a module the parse already loaded it only performs the import
into the run-time scope: the registry answers "loaded", and the module's
mainline does not run a second time. Each module therefore runs exactly once
per process, at the point rakudo would run it, and the "runs twice" consequence
of ADR-0124 and ADR-17557 disappears.

### 2.4 Errors and ordering

- A module that fails to load fails the **compile** of the importing unit at the
  `use`, as rakudo reports `===SORRY!===` there. ADR-0124 §2.4 ("a failing probe
  contributes nothing") no longer applies.
- Module side effects (output, files, `%*ENV` changes) now happen before the
  importer's later statements are parsed, which is rakudo's order. A BEGIN of the
  importer itself still runs in the prologue after the parse; interleaving the
  importer's own BEGIN blocks with the parse is not decided here (§5).
- A `use` inside a routine body that is never called still loads its module, as
  on rakudo (the prologue already does this since ADR-0134).

### 2.5 Analysis never executes

`--dump-ast`, `--dump-*`, `src/analysis/` and `crates/mutsu-lsp` parse without a
loader: they get a no-op loader that answers "not loaded", and the parser falls
back to the static scan, as the probes are gated off there today. This is the
same trust boundary docs/security.md states for the probes; it moves from the
probe call sites to the one loader seam. `-c` (compile only) is a compiling
parse, as rakudo's `-c` runs `use` and BEGIN.

### 2.6 Backtracking and memoization

The parser memoizes primaries and may re-try a statement. The loader is called
only once a `use` statement has parsed completely, and the load is idempotent
per (module spec, repository chain): a second call returns the same loaded unit
without running anything. The parser-side registration is scoped to the parse
of that statement, so an abandoned alternative registers nothing that leaks.

### 2.7 Precompilation

A precompiled module (ADR-11756/12026) is loaded from its facts: its mainline
runs, its BEGIN-time work does not run again, and the importer's parse reads its
exports from those facts. A module's precomp record already lists the units it
loaded; under this ADR those are also the units its own parse loaded, so the
record's dependency list does not change shape.

### 2.8 Slangs

A slang module is loaded like any other module. The slang host of ADR-15762 is
then the program interpreter itself rather than the probe interpreter kept
alive for the unit. Because the parser and the interpreter share one thread, the
parser → host → back-call protocol becomes nested calls on one stack instead of
two channels. This is the last slice (§6) and the one with the largest change;
until it lands, slang activation keeps its current host.

## 3. Consequences

- One module load per process: no double side effects, no double load time for
  the probed shapes.
- The parse sees real objects. Shapes that need more than a name (computed
  operator precedence, an `EXPORT` sub returning roles or a HOW) stop needing a
  rule of their own.
- Three probe paths and their gates are deleted: `parse_time_exports.rs`, the
  `dynamic_export_stash` / `begin_calls_loaded_routine` scan flags, and, in the
  last slice, the separate slang-activation interpreter. The construction
  allowlist (`make check-interp-construction`) shrinks accordingly.
- Error timing changes: a broken module is a compile error of the importer.
  A program that relied on reaching statements before a failing `use` was
  relying on a divergence from rakudo.
- The parser gains a re-entrant call into the interpreter. The interpreter's
  state must tolerate a load while a unit is mid-parse (no unit is executing
  yet; the module's own compile is a nested compile). The existing EVAL path is
  the precedent: it compiles and runs code while an outer unit's code is live.
- Startup time of a program with many `use`s is unchanged in total (each module
  still loads once), but the load moves earlier.

## 4. Alternatives

- **Keep widening the probes (route A).** Rejected as the end state: each shape
  is another gate, every probed module runs twice, and the parser still gets only
  names. ADR-17557 chose it as the short-term step.
- **Run the whole module in a probe and ship the facts back by value.**
  Rejected: it is the probe with more data, still runs everything twice, and
  objects (HOWs, closures) do not survive a copy between interpreters.
- **Parse the whole unit, then load, then re-parse what changed.** Rejected: a
  module can change how *any* later statement parses (an operator, a slang), so
  the re-parse is the whole remainder, and the first parse may already have
  failed on syntax the module introduces.
- **A second interpreter per unit that later becomes the run-time one.**
  Rejected: two interpreters must then agree on every registry, which is the
  copy problem ADR-0134 §4 rejected for BEGIN.

## 5. Open questions (answered by §7)

Kept as written when the ADR was proposed; §7 records the answers.

- **The importer's own BEGIN blocks.** rakudo runs `BEGIN` at parse time too.
  This ADR moves only `use`. A `BEGIN` that changes what a later `use` loads
  (`BEGIN @*ARGS...`, a `BEGIN` writing `%*ENV<RAKULIB>`) still runs after the
  load. Interleaving BEGIN is the remaining part of ADR-0134 §2.4 and would get
  its own decision.
- **`no precompilation` and `require`.** `require` stays a run-time load, as on
  rakudo. Whether a `use` of a source file in a `no precompilation` unit needs
  anything beyond the ordinary path is to be checked in slice 1.
- **The disk scan cache.** It is kept for the analysis paths. Whether a
  compiling parse should still consult it to answer "which names does this
  module export" before the load (to avoid the load for a `use` whose import list
  is empty) is a measurement question, not a semantic one.

## 6. Implementation plan

1. **The loader seam.** The leaf trait, the interpreter implementation, the
   no-op loader for analysis, and the call after a parsed `use`. The registry
   marks the module loaded; the prologue's `use` imports without re-running.
   The parser still reads the static scan. Pinned by a `t/` test whose module
   prints at load time: printed once, before the importer's later parse-time
   effects.
2. **Read facts instead of probing.** The parser registers the loaded unit's
   exports. `parse_time_exports.rs` and the two scan flags are deleted.
   CompUnit::Util's `t/02-re-export` / `t/05-rexport-everything` and the ADR-0124
   tests stay green.
3. **Operators and declarators by object.** Precedence and `EXPORTHOW` HOWs come
   from the loaded unit.
4. **Slangs on the program interpreter.** The slang host becomes the program
   interpreter; the separate activation interpreter is deleted.
5. **The importer's BEGIN-time statements at parse time** (§7.1). A `BEGIN` phaser of the unit
   runs when the parser reaches it, through the same loader seam, so it interleaves with the
   `use` loads in source order. Until this slice lands, slice 1 keeps the source order with the
   transitional rule of §7.1.

## 7. Amendment (2026-11-17): the open questions, answered by rakudo

The maintainer accepted this ADR with the instruction "match rakudo": wherever §5 left a question
open, mutsu does what rakudo does. Measured on rakudo 2026.09 (RakuAST frontend) with a module
`LoudA` that prints from a `BEGIN`, an `INIT` and its mainline, and an importer that prints from
`BEGIN`s around `use LoudA`:

```raku
BEGIN say "importer BEGIN 1";
say "importer mainline 1";
use LoudA;
BEGIN say "importer BEGIN 2";
INIT say "importer INIT";
say a();
```

```
importer BEGIN 1
LoudA BEGIN          # first run only: LoudA is compiled (and precompiled) here
LoudA INIT
LoudA mainline
importer BEGIN 2
importer INIT
importer mainline 1
a
```

### 7.1 The importer's own BEGIN blocks

rakudo runs each `BEGIN` of the importer when the parser reaches it, interleaved with the `use`
loads in source order: `importer BEGIN 1` comes before everything `LoudA` prints, and
`importer BEGIN 2` after it. A `BEGIN` that changes what a later `use` finds (`%*ENV`, a
repository it registers) therefore takes effect for that `use`.

**Decision:** mutsu matches this. The importer's `BEGIN` blocks become parse-time statements of
the same loader seam: the parser hands a finished `BEGIN` to the program interpreter, which runs it
at once. That is slice 5 of §6, the remaining step of ADR-0134 §2.4; after it the prologue no
longer runs `BEGIN` blocks that already ran at parse time.

**Until slice 5 lands**, loading a module at parse time must not move it ahead of a `BEGIN` that
precedes it in the unit (that `BEGIN` still runs in the prologue). The transitional rule: a `use`
loads at parse time only while no `BEGIN` phaser of the same unit precedes it; a `use` after one
keeps loading in the prologue, at its position, as before this ADR. This keeps today's order in
every case and is removed by slice 5.

### 7.2 `no precompilation` and `require`

- A `no precompilation` module is loaded from source by every process that `use`s it, so its
  `BEGIN` runs on every run (`LoudNP BEGIN` is printed on the second run too), while a
  precompiled module's `BEGIN` runs only when it is compiled. Otherwise its `use` is the ordinary
  compile-time load: its mainline runs when the parser reaches the `use`. **Decision:** no special
  path. A module mutsu serves from its precompilation cache (ADR-11756/12026) loads from its facts
  at parse time exactly as at run time today; a module that opts out is parsed, compiled and run at
  parse time.
- `require M` is a run-time load in statement order: `say "before"; require M; say "after"` prints
  `before`, `M`'s output, `after`. **Decision:** `require` stays a run-time load and does not go
  through the loader seam.
- A `use` inside a routine that is never called loads at compile time (rakudo prints the module's
  output before the unit's first `say`). Already true since ADR-0134; unchanged.
- `use M ()` (an empty import list) still loads `M` at compile time and runs its mainline.

### 7.3 The disk scan cache

rakudo has no export scan: a compiling parse always loads the module, an empty import list
included (§7.2), and reads its exports from the loaded unit. **Decision:** a compiling parse does
not consult the static scan, or its disk cache, for a module the loader loaded; it reads the loaded
unit's facts (slice 2). The scan and its disk cache stay for the analysis paths (§2.5), which never
load, and for a `use` the loader does not take (a module it cannot resolve at parse time, which
then loads at run time as before).

### 7.4 Errors and `-c`

- A module that dies while loading fails the importer's compile at the `use`:

  ```
  ===SORRY!=== Error while compiling t3.raku
  boom in Dies
  at t3.raku:3
  ------> <BOL><HERE>use Dies;
  ```

  The importer's mainline does not run (a `say` before the `use` prints nothing), the exit status
  is 1, and what the module printed before dying is printed. A `BEGIN` before the `use` has run.
- A syntax error *after* a `use` is reported after the module has run: the module's output comes
  first.
- `raku -c` runs `use` (and `BEGIN`): `LoudA` prints and then `Syntax OK`. §2.5 already treats
  `-c` as a compiling parse.

### 7.5 The analysis gate

The loader is reached only through a thread-local stack that a compiling interpreter pushes for
the extent of its parse (`run`, `-c`, a module's compile, `EVAL`). A parse with no loader on the
stack — every analysis entry point, and any thread nobody granted one — loads nothing. That makes
the seam fail-closed by construction, the property ADR-17741 §3.2 asks of the parse-time
execution gate; the `NoExecuteGuard` callers keep their guard, and the loader also refuses while
one is held.

### 7.6 Slice 1 as built (2026-11-17)

Slice 1 landed the seam and moved the load of a `use` to the parse where that
keeps every observable order rakudo's. It leaves these `use`s in the BEGIN-time
prologue, each with the slice that moves it:

- **A `use` after a compile-time effect of its unit** that mutsu still performs
  after the parse: a `BEGIN` phaser (§7.1), and a `multi` or `proto`
  declaration. The `multi`/`proto` case is the same order question as `BEGIN`:
  rakudo installs a candidate when the parser reaches it, so a candidate the
  unit declares before `use M` ranks before the ones `M` exports, and one
  declared after it ranks after them (measured on rakudo 2026.09 with two
  `trait_mod:<is>` candidates of equal signature: the earlier one wins, and the
  later one warns "Redeclaration of multi sub ... would lead to ambiguous
  dispatch"). mutsu ranks candidates by registration order, so loading `M`
  before the unit's own hoisted candidates are registered would invert it.
  Slice 5 moves these effects to the parse and drops the rule.
- **A module that imports through a `sub EXPORT` hook**, its own or the
  `&EXPORT` it took from another module's map (Slangify's). rakudo runs the
  hook once per `use`, at compile time, with the `use`'s arguments; mutsu's
  load runs it on the first load and its import runs it again, so a parse-time
  load followed by the run-time import would run it twice. Slice 2 runs the
  hook at parse time and installs the map it returned at the import.
- **A `use` with arguments or a `:if` condition**, for the same reason as the
  prologue's preload (`preloadable_module_name`). Slice 2.
- **The `use`s of a module's own compile, and of a `require`d file.** The load
  of a module snapshots the registry and the env after the module's parse
  (`load_module_inner`), to tell what the module's body registered from what
  its importer had; a dependency loaded during that parse would be counted as
  the importer's, and leak (the transitive-type and package hiding of #7555).
  A precompilation hit parses nothing, so its dependencies load in its
  prologue either way. Slice 2 takes those snapshots before the parse and gives
  a module's compile the loader too.
- **The slang activation interpreter's own loads** (ADR-0026 §2.1): which
  `define_slang` it hands the importer is decided by the load order of its
  run-time `use`s (`slang_define_slang`'s load-stack test). Slice 4 retires it.

A parse-time load prepends the unit's parse-time `use lib` paths to the search
chain for that load only; the prologue's `use lib` registers them for good, in
`$*REPO` too, at their own positions.
