# ADR-13370: A class a slang production installs reaches the importing unit by replay

- **Status**: Accepted (amended 2026-11-09: the replay is of the metamodel calls, not of the production, see §3a)
- **Date**: 2026-11-08
- **Issue**: [#13370](https://github.com/tokuhirom/mutsu/issues/13370) (Qwiratry `aggsub`)
- **Related**: ADR-15762 (slangs by execution), ADR-0026

## 1. Context

A slang production runs in the *slang host*, a separate interpreter on its own
thread, while the importing unit is still being parsed. What it computes reaches
the unit only as the RakuAST node its action attaches. Qwiratry's `aggsub`
production builds a class (`Metamodel::ClassHOW.new_type`, `add_method`,
`add_role`, `compose`) and installs it through
`$*R.current-scope.merge-generated-lexical-declaration(Implicit::Constant(:value($class)))`.

Landed with this ADR: that call records a constant, and the host returns it as a
`my constant NAME = <value>` of the importing unit (`Declaration::ResolvedConstant`
lowers to the value). This is complete for plain data (`t/lang/parsing/slang-generated-constant.t`).
It is not enough for a class: the type object names a class that exists only in
the host interpreter's registry, so `Tally.new` in the unit dies with
`No such method 'new' for invocant of type 'Tally'`.

## 2. Options

**(a) Copy the host's class definitions into the unit's interpreter.** The class
definition, its methods (closures over host environments), the parameterized
role instance and the roles' own state would all have to cross interpreters.
Closures capture host `Env`s; every captured value would need a deep copy with
identity preserved. Large and fragile.

**(b) Replay the production in the unit's interpreter at BEGIN time.** The
production's statement lowers to a node that, when the unit's BEGIN prologue
runs, re-matches the same token on the same source text and runs the same action
in the unit's own interpreter, where `$*R.current-scope` records into that
interpreter's registry directly. Side effects (a class installed in the registry,
a lexical declared) are then ordinary values of the unit's interpreter. The
action must be repeatable, which a production that only builds its result from
its match is (and a Raku action running twice is what rakudo's own parse-then-
BEGIN cycle already tolerates).

**(c) Execute productions in the unit's interpreter from the start.** The parse
runs before the unit's interpreter does, so it has no interpreter to run in.

## 3. Decision (recommended)

Option (b): the host stays the parse-time oracle (it decides where the
production ends and which node stands for it), and a production whose action
touched the resolver's generated-lexical list is replayed in the unit's
interpreter at BEGIN time to produce the real values. The generated-constant
mechanism landed here becomes the replay's output channel: the replay binds the
constants it collects in the unit's scope.

## 3a. Amendment: replay the metamodel calls, not the production

Re-running the production in the unit's interpreter does not work as the
decision above reads: the production's nested pieces (`<blockoid>`, `<signature>`,
and the `statement-prefix` hooks whose actions stash their phasers into dynamic
variables the `aggsub-def` action reads) are served by the parser thread and the
host, so a second run in the unit would need a second parser link, and the phasers
would still be stashed in the host. What the unit needs is only what the production
*built*, which is a sequence of metamodel calls on a type the hook minted.

So the host records, while a hook runs, every `new_type` and every
`add_method` / `add_private_method` / `add_multi_method` / `add_fallback` /
`add_role` / `add_parent` / `add_attribute` / `compose` whose target the hook
minted (`src/runtime/slang_class_replay.rs`). The statement the host returns for a
production that installed a generated constant whose value is such a type is
`my constant NAME = __mutsu_slang_replay_class(<log>, <type>)`: the unit applies the
same calls in its own interpreter and binds the type it minted. The method values
in the log are closures and run in the unit like any other closure; a role is
named, and the unit has loaded it (the module that installed the slang is `use`d
there too). Anything the production built outside the metamodel (a hash it
filled, a package stash) is not replayed.

## 4. Consequences

- A class built by a slang action is a first-class class of the unit.
- Actions run twice (once in the host, once at BEGIN); the host run is parse-only
  and its generated constants are discarded once the replay exists.
- `Qwiratry::Aggregate::AggSub`'s `aggsub Tally[Int]($n) {...}` can then pass
  `t/unit/aggregate-aggsub.rakutest`. Not started in this ADR's PR.
