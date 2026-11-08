# ADR-15762: Slangs by execution — production hooks into the Rust parser, user tokens and actions run on the slang host

- **Status**: Accepted (maintainer decision 2026-11-08; proposed 2026-11-07)
- **Date**: 2026-11-07
- **Deciders**: Claude (lane:rakuast worker 15), for the maintainer's review
- **Issue**: [#15762](https://github.com/tokuhirom/mutsu/issues/15762); first consumers
  [#13370](https://github.com/tokuhirom/mutsu/issues/13370) (new productions) and
  [#13369](https://github.com/tokuhirom/mutsu/issues/13369) (core action overrides). Unblocks
  [#15793](https://github.com/tokuhirom/mutsu/issues/15793) in part (see §6).
- **Supersedes**: [ADR-0026](0026-slang-activation-architecture.md) §4 ("do not execute token
  bodies") and the "recognized-override map" of §2.3 as the *only* mechanism. §2.1 (parse-time
  `use`, slang state scoped to the unit) and §2.2 (the `$*LANG` surface) stay.
- **Related**: [ADR-0091](0091-slang-package-declarators.md), [ADR-0098](0098-if-pragma-actions-slang.md),
  [ADR-10723](10723-rakuast-is-the-frontend-ir.md) §2.2 (Stage 4), [ADR-15334](15334-stage-2-parser-builds-typed-rakuast-nodes.md)
  Amendment 5, [docs/security.md](../security.md).

## 1. Context

ADR-0026 §4 rejected executing a slang's token bodies: the parser is a hand-written Rust recursive
descent, and "a Raku-grammar interpreter with Rakudo-compatible internal rule names" was out of
proportion to the one consumer then (Slang::Tuxic, three rules). Since then the answer has been a
*recognized-override map* (`src/parser/stmt/simple/slang_use.rs`, `slang_modes.rs`): a fixed list of
rule names becomes a `SlangModes` flag, and any other override is refused with
`Slang activation NYI: grammar rule override '...'`.

The consumers have changed. #13370 and #13369 record **eleven** distributions that load and pass
their tests on rakudo 2026.09 and die at activation on mutsu, each with a *different* override:
Qwiratry (`routine-declarator:sym<mold>`, `statement-control:sym<givenroot>`, a core `INFIX-EXPR`
rewrite), Slang::Roman / Slang::NumberBase (`number:sym<…>`), Slang::Otherwise
(`statement-control:sym<for>`), T, Slang::Date (`value:sym<date>`), Slang::Lambda (`lambda`),
Slang::Emoji / Slang::Nogil (`sigilless-variable`), Slang::Mosdef (`routine_declarator:sym<method>`),
Slang::SQL (`statement_control:sym<sql>`). One mode flag per rule does not scale to this list, and
none of the bodies is reducible to a flag: they are regexes that call core rules (`<block>`,
`<.ws>`, `<.end-keyword>`, `<signature>`) and actions that build RakuAST and `attach` it.

Two things make execution feasible now that it was not in August:

1. mutsu has a Raku regex/grammar engine (`Grammar`, tokens, `Match`, `make`/`.made`, `proto token`
   dispatch) that runs user grammars today.
2. The parser builds typed RakuAST nodes and `rakuast::lower` turns them into the executable tree
   (ADR-15334 Amendment 5): an `attach`ed node has somewhere to go.

What does **not** change: the base grammar stays the Rust parser. The decision is how far a slang
can reach into it, and how.

## 2. Decision

### 2.1 Production hooks, not a grammar rewrite

The Rust parser keeps parsing Raku. At each **hookable production** it consults, once per attempt
and only when the unit has registered slang candidates for that category, an ordered list of user
candidates. Hookable categories are the protos a slang can extend, named as rakudo names them
(`statement-control`, `routine-declarator`, `scope-declarator`, `package-declarator`,
`statement-prefix`, `term`, `value`/`number`, `prefix`/`infix`/`postfix`, `quote`, `identifier`,
`name`, `methodop`, `sigilless-variable`, plus the legacy underscore spellings as aliases). A
category with no registered candidate costs one flag test.

Order follows Raku: a user candidate competes with the built-in ones by longest literal prefix, ties
broken by declaration order with the mixed-in role's candidates **before** the core ones (what
`.^mixin` does to a `proto token`). The built-in candidate is represented to that comparison by the
keyword/shape the Rust parser would have matched (a probe that does not build a node).

### 2.2 The slang host

Registration already runs the slang module in a fresh interpreter on a dedicated thread
(`runtime::slang_activation`). That interpreter becomes the unit's **slang host**: it stays alive
until the unit has been parsed and owns the composed grammar and actions objects
(`$*LANG.slang_grammar('MAIN')` returns a real class `Raku::Grammar`-shaped handle; `.^mixin` is
real role composition, so `callsame`/`nextsame` between a role's token and the base work). The
parser (on its own thread) calls the host synchronously over a channel:

```
request  = (category, source: Arc<str>, byte offset, unit id)
response = Matched { to: byte offset, node: NodeData } | NoMatch | Error(msg)
```

Rationale: AGENTS.md forbids building an `Interpreter` to run code outside the listed exceptions
(parse-time module probes are one) and parse-time aliasing of the running interpreter would be
unsound; a host with a message boundary is the existing, safe shape. `NodeData` is a `Send` mirror of
`RakuAstNode` (class, fields, child nodes, and leaf `Str`/`Int`/`Num`/`Rat`/`Bool` values); a leaf
of any other type crossing the boundary is a hard error naming the field.

### 2.3 Running a user token

The host matches the candidate's regex against the unit source **from the request offset**
(`subparse`-style, with the cursor at that offset, anchored there), as a method of the composed
grammar. Core rules the token calls are **native shims** methods of the base class:

| Shim | Behavior |
| --- | --- |
| `ws`, `.ws`, `end-keyword`, `identifier`, `name`, `variable`, `number` | return the span the Rust scanner accepts at the cursor |
| `block`, `blockoid`, `statementlist`, `EXPR`, `term`, `signature`, `typename`, `trait` | RPC **back** to the parser thread: parse that production at the cursor with the parser's current scope, return a `Match` spanning it whose `.ast` is the RakuAST node the parser built |

The back-call is why the host and the parser alternate on a strict call stack (the parser blocks
while the host runs): no concurrent access to parser state, and the host's re-entry is bounded by the
nesting of the source (§5, depth bound). Shims are added **by consumer** — slice 1 needs only `ws`,
`end-keyword`, `block` — and an unknown `<rule>` is the same loud compile-time error ADR-0026 §2.2
mandates, now naming the missing shim.

### 2.4 Running a user action

Immediately after a user token matches (rakudo runs actions on match success, not at the end), the
host calls the same-named method of the composed actions object with the `Match`. `make`/`self.attach`
store the node on the match; the response carries it. A token with no action gets `Match.ast` of
`Nil`, which the parser reports as "slang production produced no node" for an expression/statement
hook (never a silent skip).

### 2.5 Re-entering the pipeline

The parser **lowers the returned node at the hook**, with the parser's current lexical view
(`rakuast::lower` with the `NameScope` of the point, #15760). The result is the `Stmt`/`Expr` the
hook point stands for (a `statement-control` hook yields a statement, a `term` hook an expression);
a node of the wrong kind is a compile error citing the category. Until #15760's NameScope covers
every declarator, names in the node resolve dynamically — a documented limit, not a fallback path.

### 2.6 Core action overrides (#13369)

A core production with a rakudo action name (`EXPR` → `INFIX-EXPR`/`PREFIX-EXPR`/...) calls the host
**only if** an actions role defines a method of that name. The core action is exposed on the base
actions class as a native method returning the node the Rust parser just built (as `NodeData`), so
`callsame` returns the core node, the user method rewrites it, and the parser uses the returned node
(lowered as in §2.5). No registered override → no call.

### 2.7 The recognized-override map becomes the migration ledger

Existing flags (`spaced_call`, `spaced_methodop`, `ident_trailing_punct`, `use_if_adverb`) keep
working in the meantime. Each is retired in its own slice by running its slang's **real token**
through §2.1–2.5 and deleting the flag; the gates are the Slang::Tuxic suite (8/8), the CSV battery,
and the Slangify fixture. A flag is not deleted before its slang passes through a hook.

### 2.8 Caching and memoization

A hook result is a pure function of `(category, offset, grammar state)`. The parser keeps a per-unit
`offset → result` table consulted before any RPC (backtracking re-asks the same position many
times), and the existing statement/term memo tables are invalidated when a slang registers (as
`invalidate_all_memos` already does).

### 2.9 Trust boundaries

Slang code is script code and runs only where a script runs. A non-executing parse (`--dump-ast`,
the analysis API, `mutsu-lsp`) never starts a host and parses in the base grammar (the existing
`no_execute` gate); the host never starts on a path that only parses. Units that activate a slang are
never precompiled. The channel is bounded and a host request has a size cap on the `NodeData` it
accepts; recursion through shims is depth-bounded and raises a catchable `X::` exception, not a
stack overflow (docs/security.md, resource bounds).

## 3. Slices

1. **Host + hook plumbing + first category.** The slang host stays alive; the request/response
   channel; `statement-control` hook; shims `ws`, `end-keyword`, `block`; the `twice` repro of #13370
   passes and `t/slang/`-style tests pin the mechanism. The mode-flag map is untouched.
2. **More categories.** `routine-declarator`, `term`/`value`/`number`, `scope`/`sigilless-variable`:
   Slang::Roman, NumberBase, Date, Lambda, Mosdef, SQL, Emoji, Nogil, Otherwise, T.
3. **Core action overrides** (#13369, Qwiratry's `INFIX-EXPR`).
4. **Qwiratry's internals** (`enter-block-scope`, `$*BLOCK.replace-*`, `key-origin`): done when
   `t/unit/mold-slang.rakutest` passes 10/10 without a mode flag per override.
5. **Retire the flags** one by one (§2.7).

## 4. Alternatives rejected

- **Rewrite the parser as a Raku grammar** (ADR-0026 §4's alternative): cost out of proportion; the
  Rust parser is the product's hot path and its error messages are tuned. Hooks give a slang the
  same reach at the productions it can actually extend.
- **More mode flags** (status quo): one Rust flag per distribution's rule; unbounded, and the
  bodies are not flags.
- **Run the host on the parser's own interpreter**: aliasing a running `&mut Interpreter` from the
  parser is unsound, and AGENTS.md forbids building another to run code outside the listed cases.
- **Serialize the node as `.raku` text and re-parse it**: lossy for anything beyond literals and
  slow per hook; `NodeData` is the same tree without the text round trip.

## 5. Risks and bounds

- **Fidelity of the shims.** A slang token may call a core rule mutsu's parser does not expose as a
  separable production. Each gap is a named, loud error and a new shim, found by the consumers.
- **Latency.** Two thread hops per hook attempt; mitigated by §2.8 and by the zero-cost-when-unused
  check. A measurement on the Slang::Date and Qwiratry tests is part of slice 1's acceptance.
- **Depth.** Shim back-calls nest with the source; the bound of §2.9 applies.

## 6. #15793 (`RakuAST::Deparse::Highlight`)

Highlight does not extend the *syntax*; it subclasses the compiler's `Actions` and `Grammar`
(`nqp::gethllsym('Raku', 'Actions' | 'Grammar')`) to observe tokens while parsing. §2.2's
`Raku::Grammar`-shaped base class and a base actions class (§2.6) are the same objects, so slice 1
and 3 give Highlight its `Actions` and `Grammar` to inherit from; whether observing every token also
needs a per-token callback (as opposed to per-hook) is **open** and decided when #15793 resumes.

## 7. Open questions

- Whether LTM between a user candidate and a built-in one needs the built-in's full token length
  (a probe) or only its leading literal, for the categories of slice 2.
- The exact `NodeData` leaf set once `RakuAST::Doc` nodes cross the boundary.
- Slang state across `EVAL` (ADR-0026 §5, first question): this ADR keeps it unit-scoped.

## 8. Amendment 1 (2026-11-07): slice 1 as built

Slice 1 (`statement-control` hook, host, channel, `block` shim) landed with these
choices, each a refinement of the text above rather than a reversal:

- **Positions are character offsets into the source from the start of the
  production to the end of the unit**, not into the whole unit: the parser hands
  the host `&input[..]` at the statement start, and a back-call parses
  `&input[pos..]`. No global source pointer is needed.
- **The host is re-entrant.** A production inside the block a shim parsed (a
  `twice` in a `twice` block) sends a `Hook` while the host waits for its
  `BackReply`; the shim serves it on the spot. Nesting is bounded
  (`MAX_HOOK_DEPTH`, a catchable error), as §2.9 requires.
- **The node is lowered where the unit is finalized, not where the production is
  parsed.** The unit's finalization (ADR-15334 Stage 2) normalizes the parser's
  spelled tree and re-converts any plain expression it does not recognize, so a
  plain lowered statement placed in the tree would be rejected. The hook returns
  `ParsedStmt::Lowered { stmt, node }`: the statement already lowered (finalize
  takes it as it is) beside the node the action attached (a conversion that reads
  the parse back as RakuAST, such as the block of an outer production, uses
  that). The name scope is the default one until #15760's NameScope reaches the
  hook (§2.5's documented limit).
- **Shims.** `block` is a back-call. `end-keyword` is rakudo's own token in the
  host's base grammar, and `ws` is the grammar's default `ws`; the Rust scanner's
  `ws` shim of §2.3 is still to come with the first slang that needs comments in
  its whitespace.
- **A token is only a hook when the slang's grammar half writes it.** An
  actions-only method naming a production nothing declares is still the
  activation error of ADR-0026 §2.2.
- Two engine bugs the first real consumer exposed were fixed on the way: an alias
  spelled like its rule (`$<block>=<block>`) filed the capture twice, and the
  `.ast` of the Match a grammar *method* answers was dropped instead of carried
  to the capture.

### Amendment 2 (2026-11-07): slice 2, term-level categories

`term`, `value` and `number` productions hook where the primary-term parser starts
(`primary_term`), ahead of the built-in term parsers; they share the host, the channel and the
`statement-control` machinery (`run_category`). The lowered expression is carried as
`ParsedExpr::Lowered { expr, node }`, the expression twin of `ParsedStmt::Lowered`. Still to come
from slice 2: `routine-declarator`, `scope-declarator`, `sigilless-variable`, which need the
`signature`/`trait` shims.

### Amendment 3 (2026-11-07): slice 3, core action overrides as a pre-finalization pass

`INFIX-EXPR` and `PREFIX-EXPR` overrides (#13369) are not run at the parser's productions: the
Rust parser builds infix applications at 34 sites with no single funnel, and the unit's
finalization (ADR-15334 Stage 2) is the one place the whole tree is normalized. So the override
runs as a pass over the parsed unit **just before** `finalize_unit` (`slang_bridge::apply_action_overrides`):
bottom-up over `Expr::Binary` / `Expr::Unary`, each application is converted to its RakuAST node,
the host composes the slang's actions role onto a base actions class whose `INFIX-EXPR` /
`PREFIX-EXPR` answer the `.ast` of the Match they are given (so `callsame` returns the core node),
runs the override and answers the node it attached; that node replaces the application
(`ParsedExpr::Lowered`, so a parent sees its child's node). The pass touches only what follows the
`use` of the activating module, and costs nothing for a unit with no override. `PREFIX-EXPR` shares
the path; other core actions (`POSTFIX-EXPR`, `statement-control:sym<given>`) are added by naming
them in `slang_action_rule`.

### Amendment 4 (2026-11-07): `refine_slang` and `numish`

`$*LANG.refine_slang($name, $grammar-role, $actions-role?)` is `define_slang` over roles given
directly (the handles `.^mixin` builds are only a recording). `numish` joins `INFIX-EXPR` and
`PREFIX-EXPR` in `slang_action_rule`: the pre-finalization pass offers every numeric `Expr::Literal`
to it. Role values are read by their declared name (a parenthesised instance `(Numish[...])` or an
anonymous `role { }` is taken as its declaration; the anonymous case is read as empty until #17008).

