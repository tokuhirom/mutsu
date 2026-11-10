# ADR-17243: A slang production's action effects outside the metamodel reach the unit by replaying the action

- **Status**: Proposed (2026-11-10; slice 1 implemented)
- **Date**: 2026-11-10
- **Issue**: [#17243](https://github.com/tokuhirom/mutsu/issues/17243) (Qwiratry `mold` / `wrapper`),
  sub-issue of [#13370](https://github.com/tokuhirom/mutsu/issues/13370)
- **Related**: ADR-15762 (slangs by execution), ADR-13370 (class replay), ADR-16888

## 1. Context

A slang production's token and action run in the slang host, an `Interpreter` on its own
thread. What reaches the unit is the RakuAST node the action attached, plus (ADR-13370 §3a)
the metamodel calls on a type the hook minted. Any other effect of the action stays in the
host: `Qwiratry::Mold::Slang`'s `mold-def` builds a `Mold` and calls `register-mold($mold)`,
which pushes onto the module-level `Qwiratry::Mold::Registry.instance`. The host's copy of
that module receives the mold; the unit loads its own copy, and the `transformer` HOW that
drains the registry when it composes a class finds nothing.

## 2. Options

1. **Replay the actions in the unit over the host's Match tree.** The host records which of the
   slang's own action methods ran, with their Match nodes; the statement the host returns for
   the production calls them again in the unit's interpreter.
2. **Share module-level state between the host and the unit.** The unit loads each module
   itself with fresh state; aliasing the host's copy is fragile (closures over host `Env`s,
   load order).
3. **Make the unit's interpreter the host** (ADR-15762's long-term end state). Removes the
   split; the parser is a pure function today, and "never build an `Interpreter` to run code"
   applies.

## 3. Decision

Option 1, narrowed to what Qwiratry needs and what is safe:

- **Recording.** While a hook runs, `call_action_method` records `(method, Match, the Match's
  `:my` dynamics)` for every action of the slang's *own* production. Core actions a slang
  overrides (`INFIX-EXPR`, `statement-control:sym<given>` and their kin, `slang_action_rule`) and
  the host's base helpers (`typename`, `attach`) are not recorded: they `callsame` into the core
  node and have no effect beyond it.
- **Where it applies.** The hook answers for the declarator and statement categories
  (`routine-declarator`, `scope-declarator`, `statement-control`, `statement-prefix`). A
  production that built a class keeps the class replay of ADR-13370 §3a, and is not also replayed.
  A `term` is never replayed: a term can run many times and has no statement to hold the call.
- **The statement.** The node the action attached is returned inside a `StatementList` beside
  `__mutsu_slang_replay_actions(@roles, @events)`; lowering turns the pair into a
  `Stmt::SyntheticBlock`, so a declaration keeps its enclosing scope (a method stays a method of
  its class body).
- **The replay.** The builtin builds an actions class from the slang's action roles (named; the
  unit has loaded the module that defines them), installs `$*R` / `$*CU` stubs shared with the host
  (`HOST_STUBS`) and each event's `:my` dynamics (`$*BLOCK`), and calls the recorded methods in the
  recorded (post-order) order. Each production replays once: a statement the unit executes again
  (a routine body) does not register twice what rakudo's one compile-time run registered once.

## 4. Consequences

- `register-mold` and any similar effect of an action (a registry push, a module-level counter, a
  `my`/`our` write of the slang's module) happen in the interpreter that runs the unit.
- Actions run twice (host, then unit). An action must be repeatable, as ADR-13370 already requires.
- The replay runs where the statement stands, not at BEGIN time as rakudo's compile-time action does:
  a unit that reads what an action registered *before* the production's position sees it unregistered.
  Making the call a `BEGIN` phaser does not help yet: the lowered production is a `Lowered` statement the
  phaser passes do not enter. TODO: hoist the call.
- The action's closures (`compiler.compile-blockoid`) are built where the unit runs, so a `mold` body
  binds the unit's lexicals when its statement runs.
- **Not covered (slice 1)**: a production in term position (`my $x = mold ...`: the node stands alone);
  a slang whose action roles only the host can name (a `my` or anonymous role, #17008): no replay,
  as before; an action that needs a parse-time-only service the unit lacks (`$*R` lexical lookup,
  `to-begin-time` on a node with unresolved names); a production whose side effect must be visible to
  *later parsing* of the same unit (that stays in the host).

## 5. Implementation notes (slice 1)

- Recording: `slang_class_replay::record_slang_action`, called from `call_action_method`; the hook's
  prelude reads it with `__mutsu_slang_action_events()` (`[method, match, dynamics, site]`).
- Replay: `slang_action_replay.rs` (`__mutsu_slang_replay_actions`); the actions class per role set is
  built once, from the stubs shared with the host (`HOST_STUBS`).
- Bridge: `slang_bridge::lower_statement` lowers a `StatementList` of several statements to a
  `Stmt::SyntheticBlock` under the production's own `Lowered` node (readers of the tree see the node).
- Verified: Qwiratry `t/unit/mold-slang.rakutest` (10/10, `raku`: the same), `transformer.rakutest`
  past its mold subtests.
