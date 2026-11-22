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

### 7.7 Slice 2 as built (2026-11-17)

Slice 2 is split in two. **2a** (this change) makes the parser read the loaded
unit and deletes the probe:

- After a `use` the loader took, the parser asks it for the unit's facts
  (`CompileTimeLoader::loaded_facts`): the routines the module exports under
  the tags the `use` imports (`DEFAULT` for none, the named tags plus
  `MANDATORY`, everything for `:ALL` or a `use` with arguments), each
  operator's `Routine.prec` record, and the module's `EXPORTHOW` declarators,
  a BEGIN-time merge included. They are registered in the `use`'s scope next
  to what the static scan registered, so block scoping is unchanged.
- `runtime/parse_time_exports.rs` and the `begin_calls_loaded_routine` scan
  flag are deleted, and with them the second interpreter and thread per
  probed `use` (the construction allowlist loses its entry). The
  `dynamic_export_stash` scan field stays, but only as what it also always
  was: a note that the scan does not know all the module's exported
  variables and types. It no longer gates anything.
- A module's static export scan no longer leaks the scanned module's own
  `BEGIN` or `multi` into the importer's "compile-time effect seen" state
  (§7.6), nor clears the importer's: a module with a `BEGIN` of its own had
  sent every later `use` of its importer back to the prologue.

**2b** widens the loader to the `use`s §7.6 still leaves to the prologue for
slice 2: a module that imports through a `sub EXPORT` hook (the hook runs at
parse time with the `use`'s arguments and its map is installed at the
import), a `use` with arguments or `:if`, and the `use`s of a module's own
compile and of a `require`d file (the load's importer snapshots move before
the module's parse). Until 2b lands, a module whose exports the scan cannot
see (a computed export stash, a BEGIN-time `EXPORTHOW` merge) is known to the
parse only through the static scan when it is `use`d in one of those
positions, or after a `BEGIN`/`multi` of the importer (slice 5).

### 7.8 Slice 2b as built (2026-11-17)

- **Hook modules load at parse time.** A module that imports through a
  `sub EXPORT` hook (its own, or the `&EXPORT` it took from another module)
  loads when the parser reaches its `use`; the hook runs then, once, with
  the `use`'s arguments. The map it returns, and the declarations it merged
  into the `use`'s block, are kept (`ModuleState::parse_time_hook_results`,
  by module and arguments, one entry per run in source order) and the
  `use`'s import in position installs them instead of running the hook a
  second time. Declarations merged into the unit (`set-unit`) are bound at
  once for a block's `use`, as the BEGIN-time preload binds them. A hook
  that attaches a phaser to its importing scope through `$*R` keeps running
  at the import, where mutsu performs the attach (`runtime::attach_target`).
  A slang module (one that `use`s Slangify) still loads at run time; slice 4
  moves it.
- **A `use` with a literal argument list** (`use M <a b>`, `use M 'x', 2`)
  loads at parse time, its values handed to the hook as the run-time `use`
  would hand them. An argument list that has to be evaluated, a `:if`
  condition and an empty import list (`use M ()`, which loads like `need`)
  still load at run time.
- **A module's own compile parses with the loader.** Its `use`s load as its
  parser reaches them. What those loads attribute to their importer is the
  module's, as when they ran in its prologue: the module is the loading
  compunit for the extent of its parse (the #7797 visibility grants), a
  `use` imports into the package the module's `unit` declaration names (the
  parser now records it, `UNIT_PACKAGE`), and the bindings the loads left in
  the shared env are parked and put back inside the module's body window.
  A module cycle closed through a `use` (`A → B → A`, even from a routine
  body) is now reported at compile time, as rakudo reports it ("Circular
  module loading detected").
- Left for later: a `require`d file's own `use`s still load in its prologue;
  a `use` inside a block that is entered more than once runs its hook again
  on each later entry (the first entry installs the parse-time map), where
  rakudo runs it once at compile time.

### 7.9 Slice 3 as built (2026-11-18)

- **Operators** (#18077): an imported operator's `Routine.prec` record
  reaches the importer's parse, so `is equiv(&infix:<imported>)` and a
  `multi` candidate of an imported operator relate to its real precedence.
- **Declarators**: the importer's parse takes a `DECLARE` declarator's kind
  from the HOW object the loaded unit bound, not from the module's source
  (`LoadedDeclarator::is_role`). A HOW that is, or inherits from,
  `Metamodel::ParametricRoleHOW` makes the keyword parse like `role`, a
  `[...]` signature included; rakudo's `package_def` decides that from the
  HOW's `archetypes.parametric`, which no other metaclass a declarator can
  name has. A declarator the module binds while it loads
  (`BEGIN EXPORTHOW::DECLARE.WHO<kw> := HOW`) is recorded as the loaded
  unit's, beside the ones its `package EXPORTHOW` declares, so the
  importer's parse knows the keyword. The analysis paths, which load
  nothing, still read only the static scan and take every declarator to
  build a class.

### 7.10 Slice 4a as built (2026-11-18)

Slice 4 is split. **4a** moves a slang module's activation into the program
interpreter; **4b** moves the slang host there and deletes the activation
interpreter.

- The compile-time loader takes the `use` of a slang module (one that `use`s
  Slangify or calls `$*LANG.define_slang`): the program interpreter loads it
  when the parser reaches the statement, with the parse's `$*LANG` bound for
  the load, so the module's `EXPORT` hook registers the slang once, at the
  point rakudo runs it. What `define_slang` recorded (rule overrides,
  package declarators, the `EXPORTHOW` declarators the hook bound) is kept
  per `use`, by module and literal arguments, and the parser reads it
  (`CompileTimeLoader::slang_activation`) instead of loading the module a
  second time in the activation interpreter. The `use` in position only
  imports.
- `define_slang` registers for the parse when it runs at the depth of the
  activating load (`RegexGrammarState::slang_activation_depth`); a
  dependency's `EXPORT` that calls it keeps it to its own compile (#17267),
  and a slang `use`d by a module's own compile registers for that compile.
- Still in the activation interpreter: a slang with a production the host
  runs (a hook, an action override, a `statementlist` wrapper; 4b), a `use`
  the loader does not take (after a `BEGIN`/`multi` of the unit, §7.6, until
  slice 5; an argument list that has to be evaluated, a `&sub` argument), and
  the re-activation of a language for a later parse (`$?LANG` seeding an
  `EVAL`, `Str.AST($slang)`) when the slang was not activated by this
  interpreter.

### 7.11 Slice 2c: a `use`'s hook runs once (2026-11-18)

Closes the 2b leftover of a `use` whose statement runs more than once.

- Each `use` statement carries its parse site (`Stmt::Use::site`, on
  `OpCode::UseModule` as `site`). A parse-time load records the map its
  module's `sub EXPORT` returned under that site, and every run of the
  statement — a loop body, a routine called again — installs it
  (`take_parse_time_hook_result` no longer consumes a sited map). The hook
  runs once per `use` statement, at its compile, as on rakudo.
- A `use` of a module an earlier `use` already loaded runs the module's hook
  at its own compile too (`run_export_hook_for_parse`) and records its map
  the same way; it used to run the hook on each run of the statement.
- An import that carries no site (a statement lowered from a RakuAST tree,
  an `EVAL` pre-run) takes the oldest recorded map once, as in 2b.

### 7.12 Slice 5a: a closed `BEGIN` of the program runs at parse time (2026-11-18)

Slice 5 is split. **5a** runs a `BEGIN` at parse time where that needs no
frame of the unit; **5b** is the rest (a `BEGIN` that reads the unit, and the
`multi`/`proto` candidate order of §7.6).

- A statement-form `BEGIN` at the top level of the **program** (not a
  module's compile, an `EVAL`, a `Str.AST` or a fragment parse) is handed to
  the loader when the parser finishes it (`CompileTimeLoader::run_begin`). The
  program interpreter lowers it on its own, compiles it as the prologue would
  (a failure is the same `X::Comp::BeginTime`, reported as the compile error
  at the `BEGIN`) and runs it. The parsed tree keeps nothing in its place, so
  the prologue does not run it again, and it does not set the §7.6 flag: a
  `use` after it still loads at parse time.
- It runs there only when its body is **closed**
  (`begin_prologue::begin_body_core_names`): compiled on its own it has no
  free lexical, calls only core routines, names only core types and terms,
  reaches no name dynamically, and declares nothing (no routine, type,
  package, `our` variable, `use`/`need`/`import`/`require`, nested `BEGIN`);
  and the unit provides none of the names it uses (`unit_provides_name`: a
  parse-time `use` has loaded but not yet imported, so an imported routine
  that shadows a core one is not bound). Such a body cannot tell the unit's
  frame is missing, so running it early is observable only in its order
  against the `use` loads, which is rakudo's.
- It runs only while **no effect the prologue runs precedes it**
  (`UNIT_PROLOGUE_EFFECT_SEEN`, set from each finished top-level statement by
  `prologue_effects::has_prologue_effect`): a `BEGIN` that did not run at
  parse time, a computed `constant` or `enum` value, a user trait on a
  declaration, a `will begin`, a `use` that did not load at parse time (`use
  lib` included; `Test` and the inert pragmas excepted), `need` or `import`.
  This is a separate flag from §7.6's: it only decides `BEGIN`, so the `use`s
  slices 1–4 load at parse time are unchanged.
- Its `use trace` hooks are numbered when it runs, from the statement attempts
  so far, which no later attempt changes.

**5b** remains: a `BEGIN` that reads the unit's lexicals or calls its routines
needs the unit's frame (or its static cells, ADR-0134 §2.2) to exist while the
parse is in progress; the `multi`/`proto` part of §7.6 needs candidate order
to follow source order across a parse-time load. Until then those keep the
transitional rule, and slice 4b (§7.10) still waits on them.

### 7.13 Slice 5b, first part: a program `multi` before a `use` (2026-11-22)

A `multi` at the top level of the **program** no longer keeps a later `use`
in the prologue (§7.6). The candidate takes its installation order from the
parse, as rakudo installs it when the parser reaches it:

- When the parser finishes such a candidate, it asks the loader for a stamp
  (`CompileTimeLoader::reserve_decl_order`: the next `decl_order`, only while
  the program's own unit is parsed) and carries it as the internal trait
  `__decl_order` (`compile_time_loader::PARSE_DECL_ORDER_TRAIT`). Its
  registrations, hoisted and in sequence, take that stamp instead of a fresh
  one. A module loaded at parse time after it stamps its candidates later, one
  loaded before it earlier, so an equally narrow tie and `.candidates` follow
  source order across the load (`parser::stmt::sub::install_order`).
- A stamp from one process means nothing to another, so a module's compile
  (precompilable) takes none, and neither does an `EVAL`, a nested scope, a
  package body, or a declaration the RakuAST finalization rebuilds rather
  than keeps (an operator with `is assoc`/`is tighter`, a symbolic name, a
  signature alternation). Those keep §7.6's rule, as does every `proto`: a
  program `proto` followed by a `use` of a bare-file module that exports the
  family resolves through the program's proto once the module loads first,
  where rakudo (and the prologue order) dispatch through the module's.
- The in-position import of a module loaded at parse time finds the
  program's earlier candidates already registered, where the prologue load
  hid them for the load's extent. A top-level import of a bare-file module
  hid the whole visible family before installing its own
  (`shadow_imported_proto_family`); it now leaves the importer's own
  candidates, which are in the `use`'s scope and which the import extends
  (`multi f(Str) {}; use M;` dispatches over both, on rakudo and on mutsu).

Measured against rakudo 2026.09: a `trait_mod:<is>` candidate declared before
`use M` wins the tie against M's equal one and one declared after loses it;
equal `multi greet(Int)` candidates of the program and the module are
ambiguous in either order; a module loaded after a program `multi` prints its
load-time output before a later syntax error is reported
(`t/modules/module-multi-before-use-candidate-order.t`).

Remaining for 5b: a `proto` (§7.14), and a `BEGIN` that needs the unit's frame.

### 7.14 Slice 5b, `proto` part (2026-11-22)

A `proto` at the top level of the program no longer keeps a later `use` in the
prologue either. It ranks no candidate, so it needs no stamp
(`parser::stmt::sub::install_order::note_proto_installed` asks the loader
whether the unit installs at parse time, `CompileTimeLoader::installs_at_parse`).
A `proto` anywhere else keeps §7.6's rule, as an unstamped `multi` does.

§7.13 kept the rule for `proto` because one case changed: a program `proto`
with a body, followed by a `use` of a bare-file module that exports the
family, dispatched through the module's candidates alone, as rakudo does, while
the `use` loaded in the prologue. Loaded at parse time, the import extends the
program's family instead, as it already did for a `unit module`. rakudo's
result there is not a rule (#18587, closed): when it merges an imported family
into a dispatcher that has a body, it builds an only-star dispatcher that
drops the proto's body or the program's own candidates, and `.file` on it dies.
With an only-star proto, written or generated, rakudo merges both candidate
lists, and so does mutsu, now in either load order
(`t/modules/module-multi-before-use-candidate-order.t`).

Remaining for 5b: a `BEGIN` that needs the unit's frame.

