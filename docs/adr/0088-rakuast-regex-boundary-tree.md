# ADR-0088: RakuAST and execution share a source-level regex tree

- Status: Accepted (static source-tree, RakuAST, execution-lowering,
  static-value-provenance, declaration-provenance, and positional-capture
  slices implemented 2026-09-12; dynamic contents and the complete
  execution-tree migration remain)
- Date: 2026-09-12
- Related: [ADR-0011](0011-rakuast-model-layer-and-phasing.md) (the RakuAST
  model layer and its bidirectional conversion),
  [ADR-0007](0007-grammar-parse-trail-matcher.md) (the cursor/trail matcher),
  [ADR-0009](0009-regex-code-assertion-execution-model.md) (inline regex code
  assertions), [ADR-0016](0016-span-based-captures-and-lazy-match.md) (lazy
  captures), [ADR-0022](0022-regex-alternation-ltm-ranking.md) (alternation
  ranking), and [ADR-0073](0073-regex-atom-candidates-are-demand-driven.md)
  (continuation-driven regex candidates)
- Addresses: GitHub issue [#8033](https://github.com/tokuhirom/mutsu/issues/8033)
  and the regex residue identified in [#8001](https://github.com/tokuhirom/mutsu/issues/8001)

## 1. Context

### 1.1 The observed boundary

mutsu parses and executes regexes today, but its RakuAST converter has no regex
node tree. The smallest reproducer is:

```raku
say Q[/test/].AST.gist;
```

Rakudo produces a `RakuAST::QuotedRegex` whose body is a
`RakuAST::Regex::Sequence` containing a `RakuAST::Regex::Literal`. mutsu
currently reports:

```text
RakuAST: `.AST` does not yet support this construct: literal Regex(...)
```

The same gap occurs for grammar declarations:

```raku
say Q[grammar G9 { regex x { \d+ } }].AST.gist;
```

Rakudo produces `RakuAST::Grammar`, `RakuAST::RegexDeclaration`, and a tree
containing `Regex::WithWhitespace`, `Regex::QuantifiedAtom`,
`Regex::CharClass::Digit`, and `Regex::Quantifier::OneOrMore`. mutsu stores the
declaration as a `Stmt::ClassDecl` whose body contains a `Stmt::TokenDecl` or
`Stmt::RuleDecl`; conversion then stops at that declaration.

This is not a missing match arm. The current internal and model
representations answer different questions:

| Source | Current internal representation | Information not recoverable from that representation |
| --- | --- | --- |
| `/test/` | `Expr::Literal(Value::Regex("test"))` | The regex atom tree and the quoted-regex form |
| `m:g/test/` | `Expr::MatchRegex(Value::RegexWithAdverbs { ... })` | The source adverb nodes and their order/spelling |
| `token x { \d+ }` | `TokenDecl` containing `Regex(":ratchet \\d+ ")` | The raw body and the fact that `:ratchet` was an execution detail |
| `rule x { "a" }` | `RuleDecl` containing `Regex(":ratchet \"a\" <.ws>")` | Which whitespace was written and which `<.ws>` was injected |
| `grammar G { ... }` | `ClassDecl` with `parents = ["Grammar"]` | The grammar declarator when the parent list alone is ambiguous |

The transformations are deliberate for execution. `token` and `rule` bodies
are normalized before the runtime matcher sees them; rules receive implicit
whitespace and tokens receive ratcheting. The matcher then parses the resulting
string into its private `RegexPattern` representation at the point where the
current package, token registry, and dynamic values are available. That
representation is the substrate for LTM measurement, lazy candidate production,
code assertions, captures, and subrule dispatch. It is not currently a
RakuAST syntax tree.

### 1.2 Rakudo's model shape

The shape was measured against Rakudo v2026.07 on 2026-09-12. Representative
outputs are:

```text
RakuAST::QuotedRegex.new(
  body => RakuAST::Regex::Sequence.new(
    RakuAST::Regex::Literal.new("test")
  )
)
```

```text
RakuAST::RegexDeclaration.new(
  name => RakuAST::Name.from-identifier("x"),
  body => RakuAST::Regex::WithWhitespace.new(
    RakuAST::Regex::QuantifiedAtom.new(
      atom       => RakuAST::Regex::CharClass::Digit.new,
      quantifier => RakuAST::Regex::Quantifier::OneOrMore.new
    )
  )
)
```

The inventory is a real syntax tree rather than an opaque pattern string. The
first static cluster includes `QuotedRegex`, `Regex::Sequence`,
`Regex::Literal`, `Regex::Quote`, `Regex::WithWhitespace`, `Regex::Group`,
`Regex::Alternation`, `Regex::Conjunction`, `Regex::QuantifiedAtom`,
`Regex::Quantifier::*`, `Regex::CharClass::*`, named and lookaround
assertions, anchors, captures, and named subrule assertions. Declaration nodes
include `RegexDeclaration`, `TokenDeclaration`, `RuleDeclaration`, and
`Grammar`. Regex adverbs are modelled as child colon-pair nodes rather than as
flags hidden inside the pattern.

The node choice has observable details that an implementation must preserve:
an ordinary `/a/` has a single `Regex::Literal` body, `/test/` may be a
`Regex::Sequence`, `m:g/test/` carries `match-immediately => True` and a `g`
adverb, and declaration bodies are wrapped in `Regex::WithWhitespace` even
when the source body has no written whitespace.

## 2. Decision

**Proposed.** Introduce a source-level structural regex tree and make it the
shared input to both RakuAST conversion and execution lowering. The existing
`RegexPattern` becomes an execution plan produced from that tree, rather than
the owner of regex syntax or the only representation of a parser-created
regex.

The intended ownership is:

```text
source
  └─► shared RegexTree
        ├─► RakuAST regex nodes
        └─► execution lowering ─► RegexPattern ─► matcher
```

`RegexTree` is a syntax/semantic IR, not a second matcher. It must retain the
information that is observable through RakuAST and that execution currently
loses when it normalizes a body:

- the structural atom tree (sequence, literal, quote, group, alternation,
  assertion, character class, quantifier, and anchors);
- source adverbs in source order, including their written names and arguments,
  plus whether the source used a match-immediately form such as `m//`;
- the raw-vs-significant whitespace distinction needed for
  `Regex::WithWhitespace`; and
- the source/declarator kind where it is not already represented by the
  surrounding statement. In particular, the parser must retain whether a
  package-like declaration was written as `grammar`, rather than infer it from
  `parents == ["Grammar"]`.

Execution-only transformations such as `:ratchet`, implicit rule whitespace,
and separator whitespace are represented as lowering policy, not inserted into
the source tree. This makes it possible to lower a `TokenDeclaration` or
`RuleDeclaration` exactly once and to render the source-level declaration back
without reverse engineering the normalized string.

The exact Rust storage may initially be split between an expression wrapper and
declaration metadata, but those fields must point at the shared structural tree;
an independent raw-source sidecar is not the long-term design. Existing
programmatically-created `Value::Regex` values without source provenance remain
valid execution values and are an explicit RakuAST conversion boundary until
their construction site can produce a `RegexTree`.

### 2.1 Read direction

The parser constructs `RegexTree` while it has the source-level delimiters,
adverbs, declaration kind, and written whitespace available. The RakuAST
converter walks that tree and constructs the existing `RakuAstNode` tree; it
does not reparse a normalized execution string. The regex parser may share
delimiter and escape lexing helpers with the main parser, but it must not call
the runtime matcher or evaluate an embedded code block.

Declaration conversion uses the source-level declaration kind and the raw body:

- `regex`, `token`, and `rule` map to `RegexDeclaration`, `TokenDeclaration`,
  and `RuleDeclaration` respectively;
- all three declaration bodies use the measured `Regex::WithWhitespace` model;
- execution-only `:ratchet` and mutsu's injected `<.ws>` are never rendered as
  user-written regex syntax; and
- grammar conversion produces `RakuAST::Grammar` and preserves its body as a
  normal package body containing declaration expressions. A class that merely
  inherits `Grammar` is not reclassified as a grammar without explicit source
  provenance.

Static regex atoms are converted structurally. Unsupported dynamic code,
interpolated runtime values, or a source form whose tree was discarded must
return the same explicit `RakuAST: ... does not yet support this construct`
error used by the rest of `convert.rs`; the converter must not guess an opaque
literal node that claims to be equivalent.

### 2.2 Write direction

The lowerer constructs `RegexTree` directly from supported RakuAST regex nodes.
A constructed RakuAST regex tree therefore returns to the same shared tree, and
the existing compiler and runtime matcher remain the only execution path:

```text
RakuAST regex tree
  └─► RegexTree
        └─► existing compiler and execution lowering
              └─► RegexPattern matcher
```

The lowering policy must preserve declaration semantics. In particular, a
`TokenDeclaration` must lower as a token declaration so the compiler applies
ratcheting once, and a `RuleDeclaration` must lower as a rule declaration so
implicit whitespace is applied once. It must not serialize those nodes as a
generic `regex` with a manually inserted `:ratchet`/`<.ws>` prefix.

The first write slice may reject code-bearing regex nodes until the existing
RakuAST statement/expression lowerer can deparse their embedded main-slang
trees. It must never execute those trees during conversion or use a VM method
fallback to make them work. Once supported, code assertions continue to obey
[ADR-0009](0009-regex-code-assertion-execution-model.md): they run only during
the real match, not while building or ranking the RakuAST tree.

## 3. Invariants

The implementation must preserve these invariants:

- **Execution semantics are unchanged during migration.** The runtime matcher
  continues to consume its lowered execution pattern and keeps the current LTM,
  cursor/trail, capture, assertion, subrule, and demand-driven candidate
  mechanisms. Introducing `RegexTree` cannot alter a match result, side effect
  count, or cache key.
- **No reverse engineering of normalization.** `:ratchet`, implicit rule
  whitespace, and separator whitespace are lowering policy applied exactly once.
  The RakuAST converter reads the shared source tree and emits the model's
  semantic wrappers directly.
- **Source spelling remains observable where Rakudo exposes it.** Adverb names,
  adverb arguments, declaration kind, match-immediately state, and the relevant
  ordering are retained. A canonicalized runtime flag is not sufficient for
  `.gist` parity.
- **Grammar identity is explicit.** `grammar G { ... }` and `class G is Grammar
  { ... }` cannot be distinguished safely from a parent list alone. The parser
  records the declarator kind before conversion.
- **The model remains walkable.** Every emitted regex construct is a registered
  `RakuAstClass` with metadata, accessors, hierarchy information, renderer
  support, and (when in the write slice) lowerer support. An opaque raw-string
  escape hatch is not a substitute for the node tree.
- **Conversion is pure with respect to user code.** Regex code blocks,
  interpolations, and declarations are represented or rejected; none is
  evaluated by `.AST`, by regex-node construction, or by LTM measurement.
- **EVAL reuses the pipeline.** Lowering a supported tree produces the existing
  `Expr`/`Stmt` form and then uses Parser -> Compiler -> VM. No new interpreter
  or runtime slow-path is introduced.
- **Missing provenance is honest.** A regex value synthesized at runtime or
  loaded from an old serialized AST may still match normally, but `.AST` must
  report an unsupported boundary when it cannot establish the source-level
  shape.

## 4. Phasing and acceptance

The initial implementation covers the static source-tree and RakuAST slices
below. The issue remains open for dynamic regex contents and for replacing the
runtime string-to-`RegexPattern` boundary.

1. **Shared tree model.** Add the `RegexTree` node family and the expression and
   declaration plumbing that can carry it without changing execution. Parser
   tests must show that the tree retains structure, adverbs, whitespace, and
   declaration kind while the existing normalized execution value remains
   byte-for-byte unchanged.
2. **Static expression regexes.** Add the `QuotedRegex` and static
   `Regex::*` node classes, beginning with literals, sequences, groups,
   alternation, character classes, and quantifiers. Pin `.AST.gist`, `.^name`,
   accessors, and smartmatch against Rakudo in a focused
   `t/rakuast/rakuast-regex.t` test.
3. **Adverbs and declarations.** Add source-order colon-pair nodes and the
   `RegexDeclaration`/`TokenDeclaration`/`RuleDeclaration`/`Grammar` cluster.
   Pin at least `/test/`, `m:i/test/`, `m:g/test/`, `grammar G { regex x { \d+ } }`,
   `token x { \d+ }`, and `rule x { "a" }`. Include the `L10N::ZH` cases from
   #8001 so the boundary is tested through a real consumer.
4. **Static lowering.** Lower constructed static RakuAST regex nodes to the
   shared `RegexTree`, then through the current compiler and execution lowering.
   Pin `EVAL` for a `QuotedRegex` and for a grammar containing a token/rule,
   including a second match to prove no duplicate ratchet or whitespace
   normalization was emitted.
5. **Execution-tree migration.** Replace the remaining string-to-`RegexPattern`
   parse boundary with lowering from `RegexTree`, retaining deferred nodes for
   code assertions, code interpolations, captures, variables, and subrules.
   Dynamic nodes are introduced only when the existing runtime context can
   supply their values without speculative execution.

6. **Dynamic regex contents.** Add code assertions, code interpolations,
   captures, variable interpolation, and subrule references only when each
   source-level expression has a safe RakuAST representation and a lowerer
   path. Acceptance must include ADR-0009 side-effect counts and the
   ADR-0073 demand-driven candidate cases; a converter-only success is not
   sufficient for these constructs.

The complete implementation is accepted only when the focused dual-oracle
tests pass, `EVAL` uses the existing execution pipeline, and the existing
regex/grammar suites remain green. The initial implementation slices may leave
individual dynamic constructs as explicit boundaries.

## 5. Alternatives considered

### 5.1 Make the current runtime `RegexPattern` the RakuAST tree

Rejected. `RegexPattern` is an execution representation: it contains token
lists, matcher atoms, quantifier state, capture machinery, and normalized
patterns. It is produced with runtime package and registry state available and
is intentionally shaped for LTM and continuation-driven matching. Making it
the model tree would couple `.AST` to the matcher, make source constructs such
as declaration kind and adverb spelling disappear, and turn a reflection
feature into a frontend rewrite. It would also put the work directly across
the invariants of ADR-0007, ADR-0009, ADR-0016, ADR-0022, and ADR-0073.

### 5.2 Reparse the normalized pattern string in `convert.rs`

Rejected. This is insufficient for the exact cases that opened #8033:
`token` and `rule` normalization is not source syntax, rule whitespace cannot
be distinguished from injected whitespace, and collapsed adverb flags cannot
recover source names or order. It would produce plausible-looking trees that
claim the wrong program, which is worse than an explicit boundary.

### 5.3 Add one opaque `Regex` leaf to RakuAST

Rejected. An opaque leaf would make `/test/` render as a string wrapper rather
than the measured `Regex::Sequence`/`Regex::Literal` tree, prevent accessors and
smartmatch from seeing the regex structure, and make constructed nodes unable
to participate in the same lowerer contract. It would move the error from the
converter into a silently-wrong model.

### 5.4 Evaluate source or Raku code while converting `.AST`

Rejected. `.AST` must be reflection, not execution. Running regex code blocks,
loading a grammar, or invoking runtime method fallbacks while building the
tree would make side effects and results depend on introspection. It would
also violate the existing Parser -> Compiler -> VM architecture and ADR-0009's
rule that measurement never executes user code.

## 6. Consequences

The positive consequence is a bounded RakuAST implementation path: one
source-level tree becomes the contract for both `.AST` and execution, while the
mature matcher can remain the execution target during migration. Read and
write slices therefore cannot quietly choose incompatible syntax, and the
eventual removal of the string-to-`RegexPattern` boundary has a concrete owner.

The cost is a new structural IR and a dedicated regex parser whose node
inventory is large. Dynamic code-bearing regexes will remain explicit boundaries
until their embedded RakuAST expressions can be represented and lowered safely.
The execution plan remains a separate lowering target because it needs runtime
package and registry state, but it no longer owns source syntax. That cost is
intentional: it keeps one regex tree honest and prevents a small `.AST` patch
from creating a second, divergent regex engine.

## 7. Implementation status

Implemented for the static source and RakuAST slices on 2026-09-12. The parser now retains a
`RegexTree` for static regex expressions and declarations, including the
`match-immediately` bit and boolean adverbs needed by `m:i` / `m:g`. The read
direction emits `QuotedRegex`, the static `Regex::*` family, declaration nodes,
and `Grammar`; the write direction lowers those nodes back through the current
compiler and VM. The focused dual-oracle coverage is in
`t/rakuast/rakuast-regex.t`.

The static execution bridge was added in the next slice. The runtime parser
lowers source trees containing literals, quotes, groups, alternation, digit
classes, and the simple quantifiers directly to `RegexPattern`, carrying the
`ratchet`, `ignorecase`, and `ignoremark` policies. Sigspace and constructs
whose meaning depends on captures, interpolation, code, or package state keep
using the established structural parser. The bridge is intentionally a
fallback-compatible step: the compiled `Value::Regex` still carries its
execution spelling, while static expression values now carry parser-produced
tree provenance alongside it. Declaration-normalized values and the remaining
string-only entry points must still migrate without rediscovering source trees
from normalized declaration text.

The following remain intentionally open: dynamic assertions and interpolation,
captures and subrules, adverbs with runtime arguments, declaration-normalized
values, and replacing string-only regex inputs with parser-produced tree
provenance throughout the remaining matcher entry points.

## 8. Static execution-lowering slice (2026-09-12)

The static subset now has one execution lowering function shared with the
RakuAST tree. `Interpreter::parse_regex` tries this lowering before the legacy
structural parser for patterns that contain no runtime interpolation. The
lowerer emits the existing `RegexPattern` and `RegexAtom` types; it does not
add a matcher or a VM fallback. Unsupported syntax, sigspace, and any literal
containing a metacharacter whose escaped/source spelling is not retained fall
back to the old parser, preserving execution semantics during migration.

The regression pin is `t/regex/regex-tree-static-execution.t`: it exercises
literal and quantified-class `EVAL` results twice, plus a ratcheted token
declaration twice, and checks rejection cases. The Rust lowering tests pin the
plan shape and the fallback boundary. This slice establishes only the static
plan conversion; captures, subrules, dynamic values, and the remaining
string-only execution entry points remain separate slices.

## 9. Static regex-value provenance slice (2026-09-12)

Parser-created static expression regexes now retain their `RegexTree` on the
regex value as it passes through the compiler and ordinary smartmatch path.
Plain regex values use the existing transparent `Regex` view, while adverb
payloads retain their existing execution flags and carry the tree alongside
them. The captured-scope representation is optional so a source-only value
does not install a synthetic lexical scope; code-bearing regexes continue to
use the established closure path.

`Interpreter::parse_regex_value` consumes that provenance for the static
execution subset and caches the resulting `RegexPattern` with a tree
fingerprint. It falls back to the established string parser for unsupported,
dynamic, synthesized, and declaration-normalized values. The matcher and its
Parser -> Compiler -> VM entry point are otherwise unchanged: this slice
removes the reparse at the value-aware single-match smartmatch boundary only.

The focused regression is `t/regex/regex-tree-value-provenance.t`, which stores
plain and adverb-bearing regexes, lowers a constructed `QuotedRegex`, and
matches each through the ordinary value path. A stale-spelling Rust test pins
that the execution plan comes from the retained tree rather than the value's
compatibility string. Other regex entry points that currently accept only a
pattern string, declaration normalization, captures, subrules, and dynamic
regex nodes remain follow-up slices.

## 10. Declaration source-whitespace and execution-provenance slice (2026-09-12)

Declaration trees now retain the source whitespace boundary that RakuAST
exposes. `WithWhitespace` marks the term before a written whitespace run; the
root declaration receives the model's implicit final wrapper, while nested
groups receive wrappers only for whitespace written inside them. This preserves
the distinction between `rule x { a[bc]d }` and `rule x { a [bc] d }` without
encoding execution-only whitespace into the source tree. Source rendering uses
the same markers, so lowering a declaration from RakuAST does not introduce
spaces between source-adjacent terms.

Declaration bodies also carry their source tree on the regex value. The named
regex smartmatch entry point consumes that value through the existing
`RegexPattern` matcher. Token and regex declarations ignore the model
wrappers; rule declarations add a `WsRule` only between terms marked with a
written whitespace boundary. The final model wrapper never consumes trailing
input, matching the established declaration normalizer. Dynamic declarations
and matcher entry points that still require a pattern string remain on the
legacy fallback path.

The focused regressions are in `t/rakuast/rakuast-regex.t`,
`t/regex/regex-tree-static-execution.t`, and
`t/regex/regex-tree-value-provenance.t`. They pin AST parity for adjacent
groups, an EVAL'd rule's execution boundary, and repeated named-rule
smartmatches.

## 11. Positional capture-group source and execution slice (2026-09-12)

The shared tree now retains ordinary positional capture groups as
`CapturingGroup`, distinct from the non-capturing `Group` used for square
brackets. The read direction emits `RakuAST::Regex::CapturingGroup`, and the
write direction accepts that node and lowers it to the existing
`RegexAtom::CaptureGroup` path. Nested source trees and quantified capture
slots therefore keep the matcher's established sub-Match and positional
numbering semantics without executing user code during conversion.

This slice intentionally covers only the ordinary `( ... )` form. Named
capture aliases, subrule assertions, variable interpolation, code
assertions, and other runtime-valued regex nodes remain explicit follow-up
boundaries. The focused regressions are in
`t/rakuast/rakuast-regex.t` and `t/regex/regex-tree-captures.t`; they pin the
model shape, constructor/EVAL lowering, capture spans, and quantified capture
iteration values.
