# ADR-16888: A slang's `statementlist` override runs on the slang host, around the parser's statement-list loop

- **Status**: Proposed
- **Date**: 2026-11-08
- **Deciders**: Claude (lane:rakuast worker 19), for the maintainer's review
- **Issue**: [#16888](https://github.com/tokuhirom/mutsu/issues/16888) (EEL)
- **Related**: [ADR-15762](15762-slangs-by-execution.md) §2.1 (production hooks), §2.3 (shims), §2.6
  (core action overrides); [ADR-0026](0026-slang-activation-architecture.md)

## 1. Context

ADR-15762 lets a slang add a production to a hookable category (`statement-control:sym<...>`,
`routine-declarator:sym<...>`, `term`, `number`, ...) and override a core *action*
(`INFIX-EXPR`, `numish`). EEL 0.3 needs a third thing: it overrides a core **grammar method**,

```raku
my $original := $grammar.HOW.find_method($grammar, 'statementlist');
my &regular = -> Mu $cursor { ... $original.($cursor) ... };
$LANG.define_slang('MAIN', $grammar.^mixin(EEL::Slang::Grammar[&regular, ...]), $actions);
# the role:  method statementlist(|) { ... $regular-statementlist($cursor) ... }
```

`statementlist` is not a category a slang adds candidates to: every statement list of the unit
(program, block, sub body) goes through it, the override *wraps* the core rule (it decides whether to
delegate), and delegation is a call back into the host grammar's own method. Neither a per-category
candidate list (§2.1) nor an action override (§2.6) expresses "run user code around the core rule".

The same shape covers the other core methods EEL wraps (`arglist`, `block`, `circumfix:sym<( )>`,
`<[ ]>`, `<{ }>`) and Qwiratry's `enter-block-scope`. The first two walls of #16888 are already
fixed (a user role named `Grammar`; helper tokens), so a probe with EEL's `EXPORT` structure now
activates.

## 2. Decision

### 2.1 A method override is a wrapper hook, registered by method name

A grammar-role *method* whose name is a core production (`statementlist`, `arglist`, `block`, ...)
registers a **wrapper hook** for that production, in the same registry as the category hooks. Only
methods named in a table of wrappable core productions are accepted (initially `statementlist`);
any other method override stays the loud ADR-0026 §2.2 error naming it.

### 2.2 The parser calls the wrapper where it would parse the production

At each point the Rust parser would parse a `statementlist`, and only when a wrapper is registered
(otherwise one flag test), it asks the host: `Hook { rule: "statementlist", source, offset }`. The
host runs the user's method with a cursor at that offset. When the method delegates —
`$original.($cursor)` / `callsame` / `nextsame` — the **shim for `statementlist` back-calls** the
parser to parse the core list at the cursor (ADR-15762 §2.3 already names `statementlist` as a
back-call shim) and returns a `Match` whose `.ast` is the node the core parser built. The host's
answer is `(end offset, node)`; the parser lowers the node (§2.5) and continues from `end`.

The wrapper is **not re-entered by its own back-call**: the back-call parses the core rule with the
wrapper suppressed for exactly that call (a per-offset reentrancy guard), as `callsame` skips the
method it is in. Nested lists inside the back-call's block do see the wrapper, as in rakudo.

### 2.3 Closures over the host method

`$grammar.HOW.find_method($grammar, 'statementlist')` returns, on the host, a code object that
performs the back-call. This is a real `Code` on the host interpreter, so EEL's `&regular` closure and
`adapter($name)` helpers run unchanged; no parser state crosses the thread boundary except the
channel messages of ADR-15762 §2.2.

### 2.4 Scoping

Like every hook, a wrapper ends with the lexical scope holding the `use` (`LexicalScope` restores the
hook table, #17119), so `{ use EEL; ... }` wraps only inside the block.

### 2.5 What this does not decide

`$LANG.set_how('event', ...)` (a metaclass for a declarator keyword; partly present as
`slang_declarator_hows`) and `nqp::dispatch('boot-code', ...)` on a method's `$!do` are separate,
independently landable pieces; neither depends on this ADR.

## 3. Consequences

- A new kind of hook (wrapper) with a reentrancy guard: the one real complexity. It is bounded by the
  source's nesting, so the ADR-15762 depth bound (`MAX_HOOK_DEPTH`) covers it.
- Cost: one flag test per statement list without a wrapper; with one, one host round trip per list.
  The per-offset result table of ADR-15762 §2.8 applies (a wrapper is a pure function of
  `(offset, grammar state)`).
- Other wrapped core methods become table rows, not designs.

## 4. Alternatives rejected

- **Re-express EEL's overrides as `statement-control` hooks.** EEL rewrites how a *list* parses (it
  recognises its own statements among the core ones and delegates the rest); as per-keyword candidates
  it would have to enumerate the core statement forms.
- **A Raku-grammar interpreter for the whole base grammar** (ADR-0026 §4's rejected option): the base
  grammar stays the Rust parser.
- **Run the override as a post-parse tree rewrite.** The wrapper changes what the parser accepts
  (EEL's own syntax inside a list), so it cannot run after the list parsed.

## Amendment 1 (2026-11-11): §2.6, the scope a wrapped list runs in

EEL's `method statementlist` parses a `pattern` body in its own language only when
`$*BLOCK ~~ RakuAST::Method`: in rakudo `method-def` enters the method's scope before it
parses the body, so `$*BLOCK` is the method being declared, with its name and the signature
written so far (the type object when none is written).

- A routine declarator of the Rust parser (`method`/`submethod`, `sub`) announces its name and
  written signature just before its body; the statement list that body starts takes the
  announcement and the hook sends it to the host with the request (`ToHost::Hook.block`). The
  host binds it to `$*BLOCK` around the slang's `statementlist`; a list with no announcement (a
  bare block nested in the body) gets a `RakuAST::Block`. The announcement is taken once, and a
  guard drops one no list took.
- A `routine-declarator` production's actions are replayed in the unit (ADR-17243) **after** its
  own statement, so a replayed `$method.meta-object` finds the method the class installed. The
  replay owner is the package the production stands in.
- In a package body the production and its replay are one `SyntheticBlock` (a
  `StatementSequence` node when the body is converted back from its tree); neither the method
  hoisting of nested blocks nor a class body treats it as a scope, so the method is installed
  where it stands.
- An `EXPORT` hook's symbols are the importing unit's: the activation interpreter names its
  importer `<slang importer>`, so the slang's own modules, which run in the host, keep the core
  routines (EEL exports a `&make` of its own).
