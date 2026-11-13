# ADR-17741: A slang `package-declarator` production that parses its own body runs on the slang host, and its effects reach the unit by replay

- **Status**: Accepted (2026-11-13, approved by the maintainer)
- **Date**: 2026-11-13
- **Deciders**: Claude (ecosystem worker 38), for the maintainer's review
- **Issue**: [#17741](https://github.com/tokuhirom/mutsu/issues/17741) (Grammar::BNF
  `bnf-grammar` declares nothing), sub-issue of
  [#13370](https://github.com/tokuhirom/mutsu/issues/13370)
- **Extends**: [ADR-15762](15762-slangs-by-execution.md) (slangs by execution: production hooks,
  the slang host, shims) to the `package-declarator` category, which its §2.1 lists as hookable but
  no slice has implemented.
- **Narrows**: [ADR-0091](0091-slang-package-declarators.md). Its declarative read stays the
  mechanism for a candidate that delegates to the core `<package_def>` / `<package-def>`. A
  candidate that parses its own body is executed instead (§2.1).
- **Amends**: [ADR-17243](17243-slang-action-effects-replay-in-the-unit.md) §3 ("a production that
  built a class ... is not also replayed"): a `package-declarator` production gets both replays, in
  a fixed order (§2.4).
- **Related**: [ADR-13370](13370-slang-installed-classes-replay-at-begin-time.md) §3a (class
  replay), [ADR-9761](9761-rakuast-subclass-native-node.md) (user subclasses of RakuAST nodes),
  [ADR-13220](13220-slang-actions-role-helper-methods.md),
  [ADR-17697](17697-thread-minted-types-are-published-process-wide.md) (alternative C),
  [docs/security.md](../security.md) (the parsing/analysis row and "Analysis never executes").

## 1. Context

### 1.1 The consumer

Grammar::BNF 1.2's `Slang::BNF` (and `Slang::ABNF`, with the same shape) adds one candidate to the
MAIN grammar and runs a second, *named* language from it:

```raku
my role Slang::BNF {
    rule package-declarator:sym<bnf-grammar> {
        <.sym><.kok>
        :my $*name;
        <longname> { $*name := ~$<longname> }
        \{ <rules=.FOREIGN-LANG('Grammar::BNF', 'main_syntax')> \}
        <.set_braid_from(self)>
    }
}
my role Slang::BNF::Actions {
    method package-declarator:sym<bnf-grammar>(Mu $/) { modern-install($/, self) }
}
sub EXPORT(|) {
    $*LANG.define_slang("MAIN", $*LANG.slang_grammar('MAIN').^mixin(Slang::BNF),
                                $*LANG.slang_actions('MAIN').^mixin(Slang::BNF::Actions));
    $*LANG.define_slang("Grammar::BNF", Grammar::BNF, Grammar::BNF-actions);
    BEGIN Map.new
}
```

- `FOREIGN-LANG('Grammar::BNF', 'main_syntax')` parses the braces' contents with the user grammar
  `Grammar::BNF` and its actions. The actions build the result grammar with
  `Metamodel::GrammarHOW.new_type(:name($*name))`, `add_method` (of tokens made by `EVAL`) and
  `compose`.
- `modern-install` (`lib/Slang.rakumod`) is user code that runs against the compiler's resolver
  `$*R`. It walks `$*R.current-package` (or `$*R.get-global` for a leading `GLOBAL::`), creates
  `Metamodel::PackageHOW` stubs for missing intermediate parts, `BIND-KEY`s the grammar into the
  stash, adopts an existing stub's stash with `nqp::setwho`, and declares a lexical for a
  single-part name or a new first part through
  `$*R.current-scope.merge-generated-lexical-declaration(RakuAST::Declaration::Import.new(:lexical-name, :compile-time-value), :resolver, :force)`.
- It attaches `Slang::Declaration.new(RakuAST::Declaration::ResolvedConstant.new(:compile-time-value($grammar)))`
  as the statement. `Slang::Declaration` is a **user subclass** of `RakuAST::Term::Declaration`
  that overrides `needs-sink-call` and `PERFORM-CHECK`.

On rakudo 2026.09 (RakuAST) the issue's repro prints `｢bar｣ foo => ｢bar｣`. mutsu prints
`No such method 'parse' for invocant of type 'A::B'`. `t/03-slang.rakutest` passes 0 of 16,
`t/05-precomp.rakutest` 0 of 2, and `t/04-abnf.rakutest` fails its last 4 of 29.

### 1.2 What mutsu already has

The decision to *run* slang tokens and actions at parse time was taken by ADR-15762 (Accepted
2026-11-08). It superseded ADR-0026 §4's "never execute a token body". Its slices built:

- the slang host (an interpreter on its own thread, alive for the unit's parse);
- hooks for `statement-control`, `routine-declarator`, `scope-declarator`, `statement-prefix`,
  `term`, `value` and `number`, and core action overrides;
- the `statementlist` wrapper of ADR-16888, and the shims (`block`, `blockoid`, `signature`, `EXPR`,
  ...).

ADR-13370 §3a replays the metamodel calls on a type a hook minted in the unit's interpreter.
ADR-17243 replays the slang's own action methods in the unit, with `$*R` / `$*CU` stubs
(`HOST_STUBS`). ADR-9761 keeps the native parent node of a user subclass of a RakuAST class.

So #13369 / #13370 (a slang role adding a `statement-control` / `routine-declarator` production
or overriding a core action) is no longer an open architectural question. Its distributions are
being worked slice by slice under ADR-15762. Those slang suites are the regression gates of this
ADR (§4).

### 1.3 What is missing for `bnf-grammar`

1. **`package-declarator` is not a hookable category** (`slang_hook_rule` has no arm for it).
   `define_slang` routes every `package_declarator:sym<…>` candidate to ADR-0091's declarative
   read. ADR-0091 registers `bnf-grammar` as a class-like keyword, and the parser then builds an
   empty class `A::B`. That is the observed error.
2. **Named languages.** `define_slang` treats every call as `MAIN` and reads only `.^mixin`
   handles. `define_slang("Grammar::BNF", Grammar::BNF, Grammar::BNF-actions)` passes a real
   grammar and records nothing. The host grammar has no `FOREIGN-LANG` / `FOREIGN_LANG`.
3. **The resolver surface.** The host's `Mutsu::Slang::Host::Resolver` has `current-scope` and
   `merge-generated-lexical-declaration` (recording only `Implicit::Constant`). It has no
   `current-package` or `get-global`, and no meaning for a generated `Declaration::Import`.
4. **Combining the replays.** The grammar is minted inside `FOREIGN-LANG`, so the class replay
   (ADR-13370 §3a) can re-create it in the unit. But the stash binding and lexical declaration
   happen in `modern-install`, outside the metamodel. Those need the action replay (ADR-17243),
   which ADR-17243 §3 does not apply to a production that minted a class. The replayed action also
   reads `$<rules>.made`, which names the *host's* grammar.
5. **A user node subclass as the statement.** The node the action attaches is an instance of a
   user class. Lowering must treat it as its native parent (`Term::Declaration` of a
   `ResolvedConstant`) whose value is the *unit's* replayed grammar.

## 2. Decision

### 2.1 `package-declarator` becomes a hook category; ADR-0091 keeps the candidates that delegate

`slang_hook_rule` accepts `package-declarator` / `package_declarator`. At `define_slang`, each
grammar-role candidate of that category is classified **by its body**:

- **It calls the core `<package_def>` / `<package-def>`** (Test::Async::Decl, the ADR-0091
  fixture `t/lib/SlangDeclarator.rakumod`, EEL's `event`): the new keyword is an alias of
  `class` / `role` with another metaclass. ADR-0091's read stays. The Rust parser parses the
  definition, and nothing runs.
- **Otherwise** the candidate parses its own body. It is registered as a hook, exactly like a
  `statement-control` candidate (ADR-15762 §2.1-2.5).

The test reads which rule the candidate *calls*, not which module declares it, so it keys on what
the slang does (the ADR-0026 §4 / ADR-0091 §4 principle). A candidate that does both is a hook. A
`package-def` shim that would let a hook delegate to the core rule is an open question (§7), not
part of this decision.

**The hook point** is where the Rust parser recognizes a package declarator: a statement that
starts with the keyword, and the same position after a scope declarator (`our` / `my` / `unit`).
The keyword is the candidate's leading literal (`<.sym>`), so the ADR-15762 §2.1 longest-literal
rule decides it without a probe. A category with no hook candidate costs one flag test.
`// Cost: O(1)` per statement start: one flag test, then one hash lookup of the identifier.

### 2.2 Named languages and `FOREIGN-LANG` run entirely on the host

`define_slang($name, $grammar, $actions?)` with `$name ne 'MAIN'` and a grammar that is a real
grammar type (not a `.^mixin` handle) **records a named language** `$name → ($grammar, $actions)`
in the host. It registers no parser hook: a named language is reachable only from a production
that names it.

The host's base grammar gets `method FOREIGN-LANG($lang, $rule, *%)` and its legacy spelling
`FOREIGN_LANG`. It looks up the named language and calls `$grammar.subparse` at the cursor (anchored,
`:rule($rule)`, with the recorded actions). It answers that Match, so `$<rules>.made` is what the
foreign actions made. No back-call to the parser is involved: the foreign grammar is an ordinary
user grammar on the host's own interpreter.

- An unknown language name is the loud compile-time error of ADR-0026 §2.2, naming it.
- `FOREIGN-LANG('MAIN', $rule)` resolves `$rule` against the host's base grammar, so it reaches
  the shims and back-calls like any other core rule. Nesting counts against the hook depth bound
  (`MAX_HOOK_DEPTH`, a catchable error).
- `// Cost: O(1)` lookup, plus the foreign grammar's own parse of the span it consumes.

### 2.3 The resolver: the host decides the *shape*, the unit performs the *effects*

`modern-install` runs twice, as every action of a declarator production does under ADR-17243:
once on the host (to produce the node the parser needs) and once in the unit (to produce the
effects the program sees). The two runs get different `$*R`s:

- **On the host**, `current-package` and `get-global` answer a fresh scratch `PackageHOW` package
  per hook. It is discarded when the hook ends. The action completes, its `BIND-KEY`s land in
  scratch, and nothing outside the hook is touched. `merge-generated-lexical-declaration` records
  both `Implicit::Constant` and `Declaration::Import`, as it records constants today.
  - The recorded **names** of `Import`s are the one thing the host contributes. The hook's
    statement declares them as lexicals of the enclosing scope, so the parser and the name scope
    (#15760) know `Solo` is a lexical before the program runs. That matches rakudo, where the
    action runs at compile time.
- **In the unit**, the replay's `$*R` (`HOST_STUBS`, shared with the host) gets real answers:
  - `current-package` is the package of the statement site. The replay call carries `$?PACKAGE` as
    an argument compiled at that site.
  - `get-global` is `GLOBAL`.
  - `current-scope.merge-generated-lexical-declaration(Import(...))` assigns the
    `:compile-time-value` to the lexical the statement already declared, refusing a name the host
    did not record (a host/unit divergence is an error, not a new lexical).

  `BIND-KEY` / `EXISTS-KEY` / `AT-KEY` then act on the unit's real stashes. `nqp::setwho` acts on
  the unit's real types. So a package the unit itself declared before the production (`class A {}`
  then `bnf-grammar A::B`) is found and extended, as on rakudo.

The resolver surface grows **by consumer**, as the shims do (ADR-15762 §2.3). Any other method
an action calls on `$*R` is a loud error naming it, never a no-op stub.

### 2.4 Replay order: classes first, then actions, through one remap table

For a production whose hook both minted types and ran the slang's own actions, the statement the
host returns is:

1. **Class replay** (ADR-13370 §3a) of every type the hook minted, including those minted inside
   `FOREIGN-LANG`, because the log is per hook. It yields the host-name → unit-value table it
   already builds (`renamed`).
2. **Action replay** (ADR-17243) of the slang's own production actions (`package-declarator:sym<bnf-grammar>`).
   Every `Value::Package` reachable from the recorded Match's `.made` / `.ast` values and its
   recorded `:my` dynamics is mapped through that table first. `$<rules>.made` is then the unit's
   grammar.

The foreign language's actions (`Grammar::BNF-actions`) are *not* replayed: they are not the
slang's production actions (ADR-17243 §3's recording rule), and their only effect is the minted
type, which step 1 covers. This amends ADR-17243 §3's "is not also replayed" for productions that
have both kinds of effect. A production with only one kind keeps exactly today's behaviour.

`// Cost: O(ops + m)`, ops = recorded metamodel calls, m = values reachable from the recorded
Matches. It runs once per production, as ADR-17243 already guarantees.

### 2.5 A user subclass of a RakuAST node is lowered as its native parent

Under ADR-9761 a `Slang::Declaration` instance retains its native `Term::Declaration` node, and
lowering uses that node. Its `ResolvedConstant`'s `compile-time-value` is mapped through the §2.4
table, so the expression evaluates to the unit's grammar.

The subclass's overrides of compiler-protocol methods are honoured where mutsu has the matching
check, and are otherwise inert, because mutsu runs no such check:

- `needs-sink-call(--> False)` suppresses mutsu's useless-use / sink warning for the statement.
- `PERFORM-CHECK` has nothing to override, since mutsu runs no `PERFORM-CHECK` phase.

Overriding a method the lowering itself consults (`IMPL-*`) is out of scope (§7).

## 3. Trust boundaries (`docs/security.md`)

### 3.1 What is trusted, and why this is not a new boundary

Running a script is trusted. A `use`d module's slang code runs **only because the script being run
`use`s it**: the host starts from the parse-time `use` of an executing unit (ADR-0026 §2.1), on
the same path that already loads and runs the module's mainline and `EXPORT`.

This ADR adds **no new execution site**:

- `FOREIGN-LANG` runs on the host, inside a hook.
- The resolver and the replay run in the unit's interpreter while it executes the unit.

The source text being parsed (the BNF body) is the script's own text. The slang code that
interprets it was chosen by the script. Nothing reaches execution that a `use` statement did not
already reach.

### 3.2 The gate on paths that only parse or analyse

Today the gate is `parser::no_execute`, a thread-local flag. It is **off by default** and set by a
`NoExecuteGuard` that each analysis entry point enters:

- `lib::dump_ast` and `lib::dump_bytecode` (`--dump-ast`, `--dump-bytecode`);
- `analysis::check` and `analysis::symbols`, which `crates/mutsu-lsp` calls;
- the static export scan.

Under the guard, `slang_use` never activates a slang, so no host starts, and the dynamic export
probe is skipped. The parse degrades: a slang's keywords stay unknown. It is never wrong in a way
that runs code.

**This ADR keeps that gate and every place it is checked.** It adds no execution on those paths.
It also proposes hardening the gate to **fail closed** (slice 2), because the opt-out shape has two
latent holes that more parse-time execution makes more costly:

- An analysis entry point that forgets the guard executes. Nothing fails, and no test notices.
- A thread an analysis path spawns starts with the default, which is "execute".

The hardened gate is a capability, not a prohibition:

- `parse_exec_allowed()` is false unless an `ExecGrant` is held on the thread.
- A grant is entered only where a script is being run:
  - the `mutsu` run path for a file, `-e` and stdin;
  - `-c`, which, like `raku -c`, runs `BEGIN` and `use`.
- A grant is passed **explicitly** to the threads that parse on a granted thread's behalf: the
  slang host thread, the parse-time export probe, and `clone_for_thread`.
- A running interpreter's own parses (`EVAL`, `require`, run-time module loads) happen on a
  granted thread and inherit the grant.
- `NoExecuteGuard` stays and *clears* the grant for its extent. An analysis caller nested inside a
  running program (a future `--dump-ast` of an `EVAL` string) is still analysis.
- A probe or host checks `parse_exec_allowed()` where it starts. `// Cost: O(1)`, one thread-local
  read per `use` that would activate.

The polarity flip removes no protection, and every existing caller of `NoExecuteGuard` keeps it.
It is therefore a hardening, which the AGENTS.md security rule welcomes. It still changes a
trust-boundary mechanism, so it is listed here for the maintainer's approval with the rest of this
ADR.

### 3.3 What `--dump-ast` and the language server do with `use Slang::BNF`

They parse in the base grammar, where `bnf-grammar` is not a keyword. Measured on 2026-11-13,
`--dump-ast` of the issue's repro fails with `Confused. Two terms in a row` at
`bnf-grammar A::B {`. That is honest: without running the slang nobody can know
the grammar. But in the language server it shows as a false error on a valid file.

Slice 3 makes the degradation explicit:

- When the static `use` scan (`contains_define_slang`, already used by activation) sees that a
  `use` would activate a slang, an analysis parse records the unit as **slang-dependent from that
  `use`**.
- `analysis::check` reports one informational diagnostic at the `use` ("slang not activated during
  analysis; the code after this line is not checked").
- `analysis::check` suppresses the parse errors after that point.
- `--dump-ast` prints the same note on stderr and keeps failing as it does now.

The analysis path never executes to recover the grammar. "Analysis never executes" stays a rule
mutsu keeps stricter than rakudo (`docs/security.md`, user decision 2026-10-23).

### 3.4 Module search and caches

- The slang module and the foreign grammar's module (`Grammar::BNF`) are found by ordinary `use`
  statements in the host, through the one search order:
  `use lib` → `-I` → `MUTSULIB` → `RAKULIB` → installed → bundled.
- `FOREIGN-LANG` takes a language *registered by `define_slang`*, never a module name to load. It
  adds no search location and loads nothing.
- A unit that activates a slang is never precompiled (ADR-15762 §2.9). `t/05-precomp`'s module
  is loaded from source on mutsu, and its cache entry, if any, is the existing private, keyed one.

### 3.5 Bounds and memory safety

- **Depth.** `FOREIGN-LANG` nesting and production nesting share `MAX_HOOK_DEPTH`, and exceeding it
  raises a catchable error (docs/security.md, resource bounds). The foreign grammar's own
  recursion is the regex engine's, which is already bounded.
- **Size.** The replay logs grow with what the slang's code does per production, which is trusted
  code. The only input-sized loop, `modern-install`'s walk over name parts, is bounded by the
  length of a name in the script's own text.
- **Threads.** The host and the parser alternate on a strict call stack (ADR-15762 §2.3), so no
  state is accessed concurrently. The named-language table and the scratch packages live in the
  host interpreter and are owned by its thread.
- Replay data crosses as `Value`s, as it does today, between two registered GC mutators.
- No `unsafe`, no `Send` / `Sync` impl, and no `&self → &mut T` access is needed.

## 4. Slices and acceptance

Each slice is one PR with a focused `t/` test.

Every slice must keep these green (the regression gates):

- Slang::Tuxic 8/8;
- the Text::CSV battery;
- Qwiratry `t/unit/mold-slang.rakutest` 10/10, plus the aggsub and transformer subtests that pass
  today;
- the EEL suites that pass today;
- `t/modules/slang-package-declarator.t` (ADR-0091) and the `t/lang/parsing/slang-*.t` family.

1. **Executed package declarators.** This slice is §2.1-2.5 together, because no part flips a test
   alone:
   - the category hook and the body classification;
   - named languages and `FOREIGN-LANG`;
   - the host and unit resolvers (`current-package`, `get-global`, `Import`);
   - the combined replay with the remap;
   - lowering of the user node subclass.

   **Acceptance:**
   - the #17741 repro matches `raku`;
   - Grammar::BNF `t/03-slang` 16/16, `t/05-precomp` 2/2 and `t/04-abnf` 29/29;
   - `t/01` and `t/02` stay green, and the distribution's ledger record flips to pass.

   A self-contained fixture (a two-rule foreign grammar plus a `package-declarator` slang that
   installs it) pins the mechanism in `t/`. It is checked under `raku` too.
2. **Fail-closed parse-time execution gate** (§3.2). It is independent of slice 1 and may land
   first.

   **Acceptance:**
   - a `t/` test with a slang module whose `EXPORT` writes a marker: `--dump-ast` and
     `analysis::check` never write it, and a normal run does;
   - a `#[test]` that a thread spawned under a `NoExecuteGuard` cannot activate;
   - every slang suite above stays green.
3. **Explicit analysis degradation** (§3.3).

   **Acceptance:** the language-server check of `t/03-slang.rakutest` reports the one
   informational diagnostic and no error.

Out of scope here, each with its own issue if a ledger dist needs it:

- a `package-def` shim (a hook that delegates to the core rule);
- `trait_mod` candidates (OO::Plugin's `trait_mod:sym<for>`; its suite tests with Test::Async,
  which is `no_baseline` under ADR-13220 §5);
- hoisting the replay to `BEGIN` time (ADR-17243's TODO).

## 5. Alternatives rejected

- **A. Extend ADR-0091's declarative read** to recognise `<rules=.FOREIGN-LANG(...)>` and bind the
  result under the longname natively. The binding logic is `modern-install`, user code. A native
  copy is the per-distribution dialect BATTERIES.md §1 bans, and it would drift from the module.
- **B. Record the host's stash effects as data and apply them in the unit** (`BIND-KEY(path, value)`
  operations instead of re-running the action). The host cannot see the unit's packages. For
  `class A {}` followed by `bnf-grammar A::B`, the host's `EXISTS-KEY('A')` is false, so it
  records a fresh stub `A`. Applying that record would clobber the unit's class. Re-running the
  action where the real packages are (§2.3) is the only faithful place.
- **C. Publish host-minted types process-wide** (an ADR-17697 option-2 style table) so that
  `$<rules>.made` resolves in the unit without class replay. It would also publish the host's
  internal types (`Raku::Grammar`, `Raku::Actions`, scratch packages) under first-wins names. It
  would couple a parse-time interpreter's registry to the program's. And the method closures would
  stay bound to host environments that end with the parse. Class replay re-creates the type in the
  unit and already exists. Revisit only if ADR-17697 is accepted with per-interpreter scoping.
- **D. Run the production in the unit's interpreter from the start** (ADR-17243 option 3,
  ADR-15762's long-term end state). The parse precedes the unit's interpreter, and AGENTS.md
  forbids building another one to run code. This is unchanged since ADR-13370 §2(c).
- **E. Treat Grammar::BNF as `no_baseline`.** It passes on rakudo's RakuAST frontend, which is
  the ecosystem oracle, so it is a real gap.

## 6. Consequences

- A slang can add a package declarator whose body is any language it registers. The binding is
  done by the slang's own code against a resolver, as on rakudo.
- ADR-0091's read and the execution path coexist, split by a structural test. A future
  `package-def` shim could retire the read the way ADR-15762 §2.7 retires mode flags.
- The action runs twice, which `modern-install` tolerates because the host run binds only scratch.
  An action whose host run has an effect outside `$*R` (a module-level registry) behaves as under
  ADR-17243: the unit run is the one the program sees.
- **Timing**: the replay runs where the statement stands, not at `BEGIN` (ADR-17243 §4). A use of
  the grammar *earlier* in execution order than its declaration (from a `BEGIN` block, or a sub
  called before the statement runs) differs from rakudo. The Grammar::BNF suites do not do this.
  The hoist is ADR-17243's open TODO.

## 7. Open questions

- Should a hook be able to delegate to the core `package-def` through a shim? That would make
  ADR-0091's read retirable.
- Exactly which positions take a package declarator after a scope declarator (`our bnf-grammar X`)?
  This will be checked against `raku` while building slice 1.
- Is the §3.3 diagnostic text right for the language server, and should `--dump-ast` grow a flag
  to say "this unit is slang-dependent" in machine-readable form?
- If ADR-17697 is accepted, should the class replay of ADR-13370 §3a and the published-types table
  converge into one "type built elsewhere, adopted here" mechanism?
