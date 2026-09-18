# ADR-0088: RakuAST and execution share a source-level regex tree

- Status: Accepted (static source-tree, RakuAST, execution-lowering,
  static-value-provenance, declaration-provenance, positional-capture,
  scalar-interpolation, named-capture, array-capture, subrule-alias, and
  bare-subrule, anchor, explicit-static-lookaround,
  named-static-lookaround, nested-static-lookaround, escaped-lookaround,
  lookaround-interpolation, array-lookaround, named-array-lookaround,
  predicate-block-code-assertion, plain-code-block, and
  interpolated-code-block, sequential-interpolated-code-block, and
  ordinary-array-interpolation, callable-interpolation,
  callable-interpolation-arguments, angle-scalar-interpolation, and
  angle-aggregate-interpolation, argumented-subrule, qualified-subrule, and
  argumented-subrule-alias, indexed-dynamic-argument, and ternary-dynamic-
  argument, modified-method-call dynamic-argument, and quoted-method-call
  dynamic-argument, dynamic-quoted-method-name argument, and hash-index
  dynamic-argument, literal-hash-index, indirect-callable, named-colonpair,
  variable-colonpair, expression-only block-valued-colonpair, hash-composer
  block-valued-colonpair, array-slurpy-placeholder, and hash-slurpy-placeholder
  dynamic-argument slices implemented
  2026-09-12 through
  2026-09-18;
  direct hash interpolation is reserved by Rakudo and mutsu;
  explicit block signatures, other complex block
  values, other dynamic contents, and the complete execution-tree
  migration remain)
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

The following remain intentionally open: dynamic assertions and non-scalar
interpolation, captures and subrules, adverbs with runtime arguments,
declaration-normalized values, and replacing string-only regex inputs with
parser-produced tree provenance throughout the remaining matcher entry points.

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
capture aliases, subrule assertions, scalar interpolation, code assertions,
and other runtime-valued regex nodes remain explicit follow-up boundaries. The
focused regressions are in
`t/rakuast/rakuast-regex.t` and `t/regex/regex-tree-captures.t`; they pin the
model shape, constructor/EVAL lowering, capture spans, and quantified capture
iteration values.

## 12. Scalar interpolation source and execution slice (2026-09-13)

Ordinary lexical scalar interpolation (`$name` and `${name}`) now has a shared
`RegexNode::Interpolation` representation. The read direction emits
`RakuAST::Regex::Interpolation` with `sequential => False` and a
`RakuAST::Var::Lexical`; the write direction accepts only that scalar form and
keeps array/hash and code-bearing interpolation as explicit boundaries.

The execution lowerer maps the node to the existing match-time
`RegexAtom::VarInterp` path. That preserves the current closure-scope and
match-time lookup behavior while avoiding a source-tree-to-string reparse for
parser-created values. Token declarations retain their source tree while the
normalized execution body continues to apply declaration policy exactly once.
Type-sensitive values and modifier-bearing patterns deliberately retain the
legacy parser path until their value-aware Unicode and runtime semantics are
part of a later slice.

The focused regressions are in `t/rakuast/rakuast-regex.t` and
`t/regex/regex-tree-interpolation.t`. They pin Rakudo's interpolation node
shape, constructed-tree lowering, stored-value lookup after lexical mutation,
type-sensitive fallback, and the declaration boundary.

## 13. Ordinary named-capture source and execution slice (2026-09-13)

Scalar named captures written as `$<name> = atom` now have a shared
`RegexNode::NamedCapture` representation. The read direction emits
`RakuAST::Regex::NamedCapture` with a string `name` and a structural `regex`
child; the default `array => False` remains an absent model field, matching
Rakudo's renderer. The write direction accepts the scalar form and rejects
array captures until their list-context semantics have their own boundary.

The execution lowerer attaches the alias to the existing `RegexToken` capture
channel. For a non-capturing quantified atom it preserves the established
whole-run scalar-alias wrapper, while an aliased positional capturing group
continues to use the matcher's per-iteration capture behavior. No subrule
lookup or code assertion is performed while converting the tree.

The focused regression is `t/regex/match/regex-tree-named-captures.t`. It pins the
source whitespace and quantified AST shapes, capture spans, constructor
accessors, and constructed-tree EVAL execution. Subrule aliases, array/hash
aliases, and code-bearing regex nodes remain separate follow-up slices.

## 14. Array-sigil capture-alias slice (2026-09-13)

Array-sigil aliases written as `@<name> = atom` now retain their list-context
bit in the shared `RegexNode::NamedCapture` tree. The read direction emits the
existing `RakuAST::Regex::NamedCapture` node with `array => True`; its accessor
remains observable even though Rakudo omits this implementation detail from
the constructor-form gist. The write direction accepts the same field and
lowers it through the existing regex value and compiler pipeline.

Execution preserves the matcher’s established distinction between aliasing a
plain atom and aliasing a positional capture group. An array alias around a
plain atom still produces one `Match`, while an alias around `( ... )` forces a
list, and a quantified capturing group produces one `Match` per iteration. The
tree lowerer pins this by setting the existing `RegexToken::force_list_capture`
only for the capturing-group forms; it does not introduce a separate matcher
path or evaluate any dynamic regex content.

The focused regression is
`t/regex/match/regex-tree-array-captures.t`. It covers source accessors and
gist parity, single and quantified parser-created aliases, and constructed
`RakuAST::Regex::NamedCapture` EVAL. Hash aliases, remaining subrule forms,
and code-bearing regex nodes remain explicit follow-up boundaries.

## 15. Ordinary subrule alias slice (2026-09-13)

Ordinary subrule aliases (`<alias=name>`) now retain their source shape in the
shared tree as `RegexNode::SubruleAlias`. The read direction emits
`RakuAST::Regex::Assertion::Alias` with an ordinary capturing
`RakuAST::Regex::Assertion::Named` child; the child `capturing` field is
present, matching Rakudo's constructor-form gist. The alias child is an
ordinary capturing named assertion, so its two capture names remain observable
through the existing model and matcher behavior.

The model and write direction also support the standalone Named assertion node
needed by the alias child and by constructed RakuAST trees. Bare and
dot-suppressed source assertions (`<name>` and `<.name>`) remain on the
existing parser path for now. This is deliberate: that path owns special
subrules such as `<same>` and grammar-local shadowing, which must not be
reduced to a generic static name lookup before those dispatch semantics have a
dedicated tree representation.

The execution lowerer maps these nodes directly to the existing
`RegexAtom::Named` path. Runtime subrule resolution, alias/original capture
sharing, dot-suppressed captures, and the Parser -> Compiler -> VM execution
pipeline therefore remain unchanged. Conversion and RakuAST construction do
not look up or execute a subrule.

This bounded slice accepts only simple, unqualified names without arguments in
the alias form. Qualified names, argumented subrules, code assertions, and
alias forms with explicit capture suppression remain fallback boundaries until
the model can retain their name-part and argument semantics. The focused
regression is `t/rakuast/rakuast-regex.t`, which pins the alias source shape,
assertion hierarchy and accessors, model construction, alias/original captures,
and end-to-end grammar EVAL execution.

## 16. Bare and dot-suppressed subrule slice (2026-09-13)

Simple bare and dot-suppressed subrule spellings (`<name>` and `<.name>`) now
retain their source form in the shared tree as `RegexNode::Subrule`, with the
`capturing` bit recording whether the dot was written. The read direction maps
both forms to `RakuAST::Regex::Assertion::Named`, omitting `capturing` for the
dot-suppressed form just as Rakudo does. The existing Named assertion model and
write direction from the alias slice therefore cover both constructed and
parser-created trees.

The execution lowerer deliberately leaves `Subrule` nodes on the existing
runtime parser path. Bare names overlap with context-sensitive assertions such
as `<same>`, `<wb>`, `<ww>`, and builtin classes, while grammar-local tokens can
shadow those names. Returning no direct execution plan for this node lets the
existing matcher resolve that package state, builtin precedence, and capture
side effects unchanged. This slice expands the RakuAST boundary without
claiming that those names are static lookup operations.

Only simple, unqualified names without arguments are included. Qualified and
argumented subrules, code assertions, and other assertion forms remain
follow-up boundaries. The focused regression is
`t/rakuast/rakuast-regex.t`, which pins both AST spellings, the default and
explicit capture flags, ordinary capture behavior, dot suppression, and a
grammar-local override of the special `same` assertion.

## 17. Static anchor slice (2026-09-13)

The four source-level anchors with existing execution representations now
retain their RakuAST shape in `RegexTree`: `^` and `$` become
`Regex::Anchor::BeginningOfString` and `Regex::Anchor::EndOfString`, while
`^^` and `$$` become `Regex::Anchor::BeginningOfLine` and
`Regex::Anchor::EndOfLine`. The parser recognizes these only as anchor
spellings; variable interpolation and other `$` forms remain separate tree
boundaries.

The read direction registers all four zero-field `RakuAST::Regex::Anchor::*`
classes, including their `Anchor`/`Atom`/`Term` hierarchy. The write direction
accepts the same nodes and lowers them through the existing compiler and VM.
String-start anchors set the existing `RegexPattern::anchor_start` policy;
line and string-end anchors use the existing zero-width matcher atoms. No
new matcher or RakuAST execution path is introduced.

The focused regressions are `t/rakuast/rakuast-regex.t` and
`t/regex/regex-tree-anchors.t`. They pin dual-oracle AST shapes, concrete
model type behavior, constructed-tree EVAL, and line/string anchor matching.
Lookaround assertions, Unicode/compound character classes, code assertions,
qualified or argumented subrules, and other runtime-valued assertions remain
explicit follow-up boundaries.

## 18. Explicit static lookaround slice (2026-09-13)

Explicit positive and negative lookaround assertions with a static body now
retain their source shape in `RegexTree`: `<?before body>`, `<!before body>`,
`<?after body>`, and `<!after body>`. The read direction emits
`Regex::Assertion::Lookahead` around `Assertion::Named::RegexArg`, preserving
the `before`/`after` name and the `negated` bit exactly as Rakudo exposes
them. The write direction accepts the same two model nodes and lowers them
back to the existing `RegexAtom::Lookaround` matcher.

The execution lowerer builds the nested `RegexPattern` from the shared tree,
so lookahead remains zero-width and lookbehind still uses the existing bounded
start search. No code is evaluated while converting or lowering the node.
The bounded parser slice accepts only pure static lookaround bodies; escaped
forms, interpolations, subrules, captures, code assertions, and the unprefixed
or dot-prefixed spellings remain on the legacy path until their distinct
RakuAST shapes and runtime context are represented.

The focused regressions are in `t/rakuast/rakuast-regex.t` and
`t/regex/regex-tree-lookaround.t`. They pin the four dual-oracle model
shapes, node accessors and hierarchy, constructed-tree lowering, repeated
lookahead use, and positive/negative lookahead/lookbehind matching.

## 19. Unprefixed and dot-prefixed static lookaround slice (2026-09-13)

Unprefixed and dot-prefixed static `before`/`after` assertions now retain their
source tree as `Regex::Assertion::Named::RegexArg`. Rakudo exposes the same
model shape for `<before body>` and `<.before body>`; the source distinction is
the inherited `capturing` flag, which is true for the unprefixed form and false
for the dot-prefixed form. The shared tree keeps that flag instead of reducing
both spellings to the explicit `Lookahead` wrapper used by `<?...>`/`<!...>`.

Execution lowering still uses the existing zero-width `RegexAtom::Lookaround`.
For a capturing named assertion it attaches the `before` or `after` name to
the outer token, producing the same zero-width named `Match` as the legacy
parser; dot-prefixed assertions leave that capture channel unset. Constructed
`RegexArg` nodes accept the same `capturing` field and lower through the normal
Parser -> Compiler -> VM path.

The bounded parser accepts only static bodies already supported by the explicit
lookaround slice. Escaped, interpolated, nested, code-bearing, qualified, and
argumented bodies remain deferred. The focused regressions pin all four source
spellings, `.capturing`, zero-width capture spans, capture suppression, and
constructed-tree execution.

## 20. Nested static lookaround slice (2026-09-13)

Static lookaround bodies may now contain another lookaround inside a regular
static group, for example `<?before [<?before bar>]>`. The parser's source
scanner balances nested angle-bracket assertions before handing the complete
body to the shared tree parser; it no longer stops at the inner `>` or falls
back to a normalized execution string. The existing `Group` and `Lookaround`
nodes therefore compose recursively in both RakuAST conversion and execution
lowering.

This slice remains deliberately static: unsupported escapes, interpolations,
code-bearing assertions, qualified or argumented subrules, and runtime-valued
bodies still fall back to their established boundaries. The focused
regressions pin the dual-oracle nested `Lookahead`/`RegexArg`/`Group` shape,
parsed and constructed EVAL, reuse, and rejection of a mismatching suffix.

## 21. Escaped lookaround slice (2026-09-13)

Lookaround bodies now accept the already-supported escaped digit class (`\d`).
The parser scanner skips escaped characters while balancing the assertion's
closing `>`, then delegates the nested body to the same `RegexTree` parser used
outside lookarounds. This keeps the RakuAST shape measured by Rakudo: `\d+`
remains a quantified `CharClass::Digit` under
`Assertion::Named::RegexArg`.

The existing execution lowerer handles the digit class without a new matcher
path: it becomes the current `CharClass::Digit` atom. Escaped classes not yet
represented by the shared tree (such as `\w` and `\s`), interpolations, code
assertions, captures, subrules, and qualified or argumented forms remain
explicit boundaries.

The focused regressions are in `t/rakuast/rakuast-regex.t` and
`t/regex/regex-tree-lookaround.t`. They pin the dual-oracle AST shape,
zero-width matching, rejection and reuse for `\d+`.

## 22. Scalar interpolation in lookaround slice (2026-09-14)

Ordinary scalar interpolation is now accepted inside the existing lookaround
assertion nodes. The source tree retains `RegexNode::Interpolation` beneath
`Lookaround`/`NamedLookaround`, so RakuAST emits the same
`Assertion::Named::RegexArg` and `Regex::Interpolation` shape as Rakudo.

Execution lowering reuses the existing `RegexAtom::VarInterp` matcher inside
the zero-width lookaround plan. The value is read at match time from the
current lexical environment, preserving reassignment for both parser-created
regex values and constructed RakuAST trees. Sequential interpolation, code
assertions, captures, subrules, and other runtime-valued bodies remain outside
this bounded slice.

## 23. Sequential scalar interpolation in lookaround slice (2026-09-14)

The shared tree now distinguishes `||` from ordinary `|` as
`RegexNode::SequentialAlternation`. Its first scalar interpolation branch
retains `sequential => True`, so a body such as `<?before bar || $value>`
converts to Rakudo's `RakuAST::Regex::SequentialAlternation` shape rather than
falling back to the normalized execution spelling. Mixed `|`/`||` expressions
are grouped according to their RakuAST precedence, and the interpolation flag
is applied only to the first atom after `||`.

Execution lowering maps the new node to the existing
`RegexAtom::SequentialAlternation` matcher and continues to read scalar values
at match time through `RegexAtom::VarInterp`. This slice is limited to scalar
variables; array interpolation, code interpolation, and other runtime-valued
branches remain deferred until their source and execution semantics can be
represented together.

## 24. Direct array lookaround slice (2026-09-14)

Direct array lookahead assertions (`<?@name>` and `<!@name>`) now retain their
source shape as `Regex::Assertion::InterpolatedVar` beneath the existing
`Regex::Assertion::Lookahead` node. The ordinary named regex-argument form
(`<?before @name>`) remains outside this slice because its nested array
interpolation is a different runtime boundary from the existing direct array
assertion parser path.

The write direction accepts constructed `InterpolatedVar` assertions and
lowers them back to the original source spelling. Execution deliberately
uses the established array-variable lookahead matcher rather than freezing the
array into a cached `RegexPattern`: Rakudo rereads the array after regex
construction, so reassignment before a later match must be visible. The
source-tree cache therefore bypasses its static-plan entry for this node while
the normal Parser -> Compiler -> VM matcher remains unchanged.

The focused regression is `t/regex/regex-tree-array-lookaround.t`. It pins the
dual-oracle `InterpolatedVar` shape, positive and negative zero-width matching,
array reassignment, constructor accessors, and constructed-tree EVAL. Named
regex-argument array interpolation, code assertions, and other runtime-valued
lookaround bodies remain separate boundaries.

## 25. Named regex-argument array interpolation slice (2026-09-14)

Array interpolation in a named lookaround's regex argument, such as
`<?before @name>` and `<!before @name>`, now retains its source shape as
`RakuAST::Regex::Interpolation` beneath
`Assertion::Named::RegexArg`/`Assertion::Lookahead`. The shared tree keeps the
aggregate `@` sigil distinct from the existing direct `InterpolatedVar`
lookaround node, while the read and write directions use the same
`Regex::Interpolation` model as scalar interpolation.

Execution preserves the bare array spelling while the nested lookaround body
is parsed, then routes it through the established `<@name>` array-variable
matcher. The resulting plan is not cached because the array is read from the
current match environment at parse time; reassignment after regex construction
therefore remains visible. The closure path now carries the parser-produced
source tree along with the defining lexical scope so this behavior survives
regex values captured into a variable.

The focused regression is `t/regex/regex-tree-named-array-lookaround.t`. It pins
the dual-oracle AST shape, positive and negative lookahead matching, live array
reassignment, interpolation accessors, and constructed-tree EVAL. Code
assertions and other runtime-valued lookaround bodies remain separate
boundaries.

## 26. Predicate-block code assertion slice (2026-09-14)

Predicate-block assertions (`<?{ ... }>` and `<!{ ... }>`) now retain their
source structure as `RakuAST::Regex::Assertion::PredicateBlock`, including the
negation field and the nested `RakuAST::Block`. The parser stores both the
original source text and the parsed statement body in the shared regex tree;
the read direction converts the body to a RakuAST block and the write
direction lowers a constructed block back to the same tree node.

Execution carries the parsed body into the existing ADR-0009 inline-code
matcher. It therefore preserves the current real-match versus declarative
probe behavior, including once-per-match side effects, without evaluating code
during `.AST` conversion or adding a VM fallback. Interpolated blocks
(`<{ ... }>`), plain code blocks, and other runtime-valued regex bodies remain
deferred until their source and execution semantics can be represented
together.

The focused regression is `t/regex/regex-tree-code-assertion.t`. It pins the
dual-oracle AST shape, nested lookaround, polarity, side-effect count, and
constructed-tree EVAL behavior.

## 27. Plain regex code block slice (2026-09-14)

Plain regex code blocks (`{ ... }`) now retain their source structure as
`RakuAST::Regex::Block`, with a positional `RakuAST::Block` child. The parser
stores the original block text and parsed statement body in the shared regex
tree; the read direction converts that body to `Regex::Block`, and the write
direction lowers a constructed block back to the same tree node.

Execution lowers the node to the existing ADR-0009 inline-code matcher as a
non-assertion code atom. This preserves the distinction between a block that
executes as part of a match and a predicate assertion, including nested
lookarounds, without evaluating code during `.AST` conversion or adding a VM
fallback. Legacy P5-style quantifier spellings remain on the existing parser
path. Code interpolation and other runtime-valued regex bodies remain deferred
until their source and execution semantics can be represented together.

The focused regression is `t/regex/regex-tree-code-block.t`. It pins the
dual-oracle AST shape, nested lookaround, execution polarity, side-effect
count, and constructed-tree EVAL behavior.

## 28. Interpolated regex code block slice (2026-09-14)

Interpolated code blocks (`<{ ... }>`) evaluate their result as a regex rather
than making a zero-width predicate decision. They now retain their source
structure as `RakuAST::Regex::Assertion::InterpolatedBlock`, including the
nested `RakuAST::Block` and the measured `sequential => False` field. The
existing `<!{ ... }>` predicate spelling remains
`Regex::Assertion::PredicateBlock`.

The parser stores the original code and parsed statement body in the shared
regex tree. Execution lowers that node to the existing
`RegexAtom::ClosureInterpolation` matcher and passes the parsed body into its
scratch interpreter, so constructed trees and parser-created regexes share the
same closure-interpolation path without evaluating code during `.AST`
conversion or adding a VM fallback.

The focused regression is `t/regex/regex-tree-interpolated-block.t`. It pins
the dual-oracle AST shape, node accessors, match-time lexical reassignment,
returned-pattern execution, and constructed-tree EVAL. Sequential/dynamic
variants and other runtime-valued regex bodies remain deferred.

## 29. Sequential interpolated regex code block slice (2026-09-14)

An interpolated code block immediately following a sequential alternation
separator, such as `foo || <{ "bar" }>`, now retains Rakudo's
`RakuAST::Regex::Assertion::InterpolatedBlock` with `sequential => True` under
`RakuAST::Regex::SequentialAlternation`. Ordinary `<{ ... }>` blocks continue
to report `sequential => False`, and the flag is scoped to the first atom in
the branch after `||`.

The parser passes the sequential-branch context through the shared regex tree
instead of treating every interpolated block as an ordinary branch. The
execution lowerer uses the existing `RegexAtom::ClosureInterpolation` atom
for the branch, and the existing `RegexAtom::SequentialAlternation` matcher
supplies branch priority. No code runs during `.AST` conversion and no new VM
fallback is introduced. Parser-created and constructed RakuAST trees
therefore retain the measured source shape while preserving match-time lexical
reassignment and sequential branch priority.

The focused regression is `t/regex/regex-tree-interpolated-block.t`. It pins
the dual-oracle sequential-alternation shape, the `True` field, branch
fallback, match-time reassignment, and constructed-tree execution. Predicate
blocks, array/code interpolation, and other runtime-valued regex bodies remain
separate boundaries.

## 30. Ordinary array interpolation slice (2026-09-14)

Ordinary aggregate interpolation in a regex body, such as `@parts`, now
retains Rakudo's `RakuAST::Regex::Interpolation` shape with the aggregate
lexical sigil and the measured `sequential` field. The same node is emitted
for an interpolation after `||`, where the field is `True` and the node sits
under `RakuAST::Regex::SequentialAlternation`.

The parser retains the source tree for these values, while execution continues
through the existing runtime regex parser. Array contents are read at match
time and the tree is never entered into the static execution-plan cache, whose
key does not include array contents. Constructed RakuAST trees therefore share
the same live array behavior without evaluating or snapshotting the array
during `.AST` conversion.

The focused regression is `t/regex/regex-tree-array-interpolation.t`. It pins
the dual-oracle AST shape, sequential field, direct and constructed matching,
match-time reassignment, and the existing named-lookaround boundary. Hash
interpolation, code interpolation, and other runtime-valued regex bodies
remain separate boundaries.

## 31. Hash interpolation remains a reserved boundary (2026-09-14)

Direct hash interpolation is not a missing regex-tree node in the current
Rakudo language. Rakudo rejects `/%var/` and `m/%var/` with
`X::Syntax::Reserved`; it also rejects a scalar interpolation whose runtime
value is a `Hash` with the same exception. The same cases are already pinned
by `roast/S05-interpolation/regex-in-variable.t` and
`t/regex/regex-tree-interpolation.t`.

The source-level consequence is deliberate: a direct `%name` spelling must
not be added to `RegexNode`, and the RakuAST converter must not invent a hash
interpolation model node for syntax Rakudo rejects. A scalar source such as
`$hash` may still retain the ordinary `Regex::Interpolation` shape because
its type is dynamic, but execution must keep the existing runtime reservation
check rather than snapshotting or lowering the hash as a regex.

This slice therefore changes no parser or matcher behavior. It settles the
hash-interpolation remainder as an explicit language boundary; if Rakudo
later assigns semantics to hash interpolation, that syntax needs a fresh
measurement and a new ADR-0088 slice. At the time of this slice, the next open
dynamic boundary was code interpolation.

## 32. Argument-less callable interpolation slice (2026-09-14)

Argument-less callable interpolation (`<&name>` and the empty-call spelling
`<&name()>`) retains Rakudo's
`RakuAST::Regex::Assertion::Callable` shape with a lexical `&name` callee.
Both spellings intentionally normalize to the same source tree.

The parser keeps the source provenance in `RegexNode::Callable`. Execution
lowering deliberately leaves the node on the existing runtime parser path, so
this slice does not invent a new VM dispatch route or evaluate a callable while
converting `.AST`. Constructed Callable nodes with an empty argument list lower
back through the same regex value path.

The focused regression is `t/regex/regex-tree-callable.t`. It pins the dual-
oracle AST shape for both spellings, the callee and optional-argument
accessors, sequence nesting, construction, and the EVAL round trip.

## 33. Callable interpolation with arguments (2026-09-14)

Callable interpolation with non-empty arguments, such as `<&name("value")>`
and the colon form `<&name: "value">`, now retains Rakudo's
`RakuAST::Regex::Assertion::Callable` node with a non-empty
`RakuAST::ArgList`. The parser reuses the ordinary call-argument parser for
the expression tree and retains the exact argument source needed by the
existing runtime regex parser; colon syntax is normalized to the same model
as parenthesized syntax.

The read direction converts every parsed argument into the `ArgList` child,
and the write direction lowers positional child expressions back into the
shared callable node. `RakuAST::ArgList.new(...)` now accepts positional
RakuAST nodes, so a constructed callable can be lowered and evaluated as a
regex as well. Execution remains on the established runtime parser path, and
hand-built arguments outside the bounded source-rendering subset are rejected
by the lowerer rather than rendered incorrectly.

The focused regression is `t/regex/regex-tree-callable.t`. It pins the exact
dual-oracle tree, colon normalization, argument-list construction, constructed
regex EVAL, and parser-created matching behavior.

The remaining open dynamic boundaries are code interpolation and other
runtime-valued regex bodies.

## 34. Angle scalar regex interpolation slice (2026-09-14)

Angle scalar interpolation (`<$name>`) now retains Rakudo's
`RakuAST::Regex::Assertion::InterpolatedVar` shape, with the lexical scalar
and the `sequential` branch flag. This is intentionally distinct from bare
`$name` interpolation: the angle form reads the current value and reparses it
as a nested regex, isolating the nested regex's captures from the outer match.

The read and write directions share the existing assertion model class for
the scalar lexical form. The execution lowerer deliberately leaves this node
on the established runtime parser path, where strings, Regex values, and
other value-sensitive cases retain their existing semantics. Because the
runtime plan depends on the current lexical value, source-tree plan caching is
bypassed for trees containing this node; parser-created regexes therefore
observe reassignment before a later match. Constructed nodes lower through the
same `Regex` value and Parser -> Compiler -> VM path without evaluating the
lexical during AST conversion.

The focused regression is
`t/rakuast/rakuast-regex-stored-interpolation.t`. It pins the dual-oracle
assertion shape, sequential alternation, Regex and string reassignment,
lookaround composition, and constructed-node EVAL. Callable interpolation,
hash interpolation, and other runtime-valued regex bodies remain separate
boundaries.

## 35. Angle aggregate regex interpolation slice (2026-09-14)

Indirect aggregate interpolation (`<@name>` and `<%name>`) now retains
Rakudo's `RakuAST::Regex::Assertion::InterpolatedVar` shape, including the
aggregate sigil on its lexical variable and the `sequential` branch flag.
This is distinct from ordinary `@name` interpolation, which remains a
`RakuAST::Regex::Interpolation` node, and from direct `%name` interpolation,
which Rakudo reserves.

The shared tree records the angle form as a value interpolation with its
sigil. Execution deliberately stays on the existing runtime parser path:
array assertions resolve current elements at match time, while hash-valued
assertions retain the runtime parser's existing semantics and diagnostics.
The source-tree execution-plan cache is therefore bypassed for all three
angle sigils, including the already-supported scalar form. Constructed
`InterpolatedVar` nodes lower through the same Regex value and Parser ->
Compiler -> VM path without evaluating the aggregate during AST conversion.

The focused regression is `t/regex/regex-tree-angle-interpolation.t`. It pins
the dual-oracle array and hash assertion shapes, sequential context, nested
lookaround, array reassignment, and constructed-node EVAL. Qualified and
argumented subrules remain deferred to separate boundaries.

## 36. Qualified regex subrule slice (2026-09-14)

Qualified subrule calls such as `<G::foo>` and `<.G::foo>` now retain
Rakudo's `RakuAST::Regex::Assertion::Named` shape. Their `Name` child stores
one `RakuAST::Name::Part::Simple` node per `::`-separated segment and renders
through `RakuAST::Name.from-identifier-parts(...)`, rather than hiding the
qualification inside one opaque string. Short aliases such as
`<alias=G::foo>` retain the same qualified assertion child.

The parser accepts only ordinary identifier segments for this slice. The
existing package-aware runtime matcher still resolves the written name
relative to the active package, and the execution lowerer emits the same
qualified spelling for constructed RakuAST trees. No new matcher or dispatch
path is introduced. Qualified subrules nested inside unsupported lookaround
bodies, argumented subrules, and long names on the alias side remain explicit
follow-up boundaries.

The focused regression is
`t/rakuast/rakuast-regex-qualified-subrules.t`. It pins the positive and
dot-suppressed AST shapes, qualified name-part accessors and construction,
qualified alias lowering, grammar EVAL, and repeated execution.

## 37. Argumented regex subrule slice (2026-09-15)

Argumented subrule calls such as `<word("a")>`, `<.word: "a", 2>`, and
`<G::word("a")>` now retain Rakudo's
`RakuAST::Regex::Assertion::Named::Args` shape. The assertion owns an
`RakuAST::ArgList` child, while the `Name` child continues to preserve
qualified segments and the optional `capturing` field keeps the positive and
dot-suppressed forms distinct.

The parser reuses the ordinary call-argument parser for both parenthesized and
colon forms. The source tree retains argument provenance for the existing
package-aware runtime matcher, and the RakuAST lowerer renders the supported
static expression subset back into the same subrule spelling. Empty argument
lists remain an argumented assertion so their explicit call boundary is not
lost. Execution therefore stays on the established Parser -> Compiler -> VM
path; no new VM dispatch or match-time argument evaluation is introduced by
the AST conversion.

The focused regression is
`t/rakuast/rakuast-regex-argumented-subrules.t`. It pins parenthesized and
colon AST shapes, qualified names, argument-node accessors, empty calls,
direct grammar matching, AST EVAL, and constructed-tree execution.
Qualified subrules inside unsupported lookarounds, argumented subrule aliases,
and other dynamic argument expressions remain separate boundaries.

## 38. Argumented subrule alias slice (2026-09-15)

Argumented subrule aliases such as `<alias=word("a")>` and
`<alias=.word("a")>` now retain the alias node around a
`RakuAST::Regex::Assertion::Named::Args` child. The child keeps its qualified
`Name`, positional `ArgList`, and capture-suppression state, while an empty
argument list remains distinct from an argument-less alias.

The shared tree records the target's dot suppression and argument provenance.
Read conversion emits the same nested model shape measured from Rakudo, and
write conversion accepts both `Named` and `Named::Args` assertion children.
The existing package-aware runtime parser still receives the alias spelling,
including its arguments, so alias and original capture behavior remains on the
established Parser -> Compiler -> VM path.

The focused regression is
`t/rakuast/rakuast-regex-argumented-aliases.t`. It pins positive and
dot-suppressed AST shapes, qualified names, empty calls, constructor support,
parser-created captures, and a grammar lowered from RakuAST. Argumented
aliases with dynamic argument expressions, code-bearing targets, and other
runtime-valued forms remain explicit follow-up boundaries.

## 39. Method-call dynamic argument slice (2026-09-15)

Ordinary method-call expressions in argumented subrules, such as
`<word($value.uc)>`, now retain their existing RakuAST expression tree. The
parser and read converter already represented the method as
`ApplyPostfix`/`Call::Method`; the missing boundary was the write direction's
small source renderer, which previously rejected `Expr::MethodCall` when a
converted RakuAST tree was lowered back to the existing regex parser.

This slice accepts only ordinary named method calls without a method modifier
or quoted method name. The renderer recursively accepts the same argument
expression subset already supported by `expression_source`, and emits a
parenthesized method call so the established match-time regex argument
evaluator can execute it. No method is invoked while converting `.AST`, and no
new matcher or VM path is introduced. Ternaries, modified or quoted method
calls, and other dynamic argument expressions remain explicit follow-up
boundaries.

The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`. It pins the method-call AST
nodes, RakuAST EVAL lowering, direct grammar matching, and lexical
reassignment between matches.

## 40. Indexed dynamic argument slice (2026-09-15)

Ordinary indexed expressions in argumented subrules, such as
`<word(@values[$index])>`, now lower from their existing
`ApplyPostfix`/`Postcircumfix::ArrayIndex` RakuAST shape back to the regex
parser. The expression renderer recursively emits the target and index using
the positional bracket spelling, leaving evaluation to the established
match-time subrule-argument path. Consequently both the array and the index
continue to observe lexical reassignment between matches.

No index is evaluated during `.AST` conversion or RakuAST lowering, and this
adds no matcher or VM path. Ternaries, modified or quoted method calls, and
other dynamic argument expressions remain separate boundaries. The focused
regression is `t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the index
node shape, RakuAST EVAL lowering, direct grammar matching, and dynamic index
reassignment.

## 41. Ternary dynamic argument slice (2026-09-15)

Ordinary ternary expressions in argumented subrules, such as
`<word($which ?? 'a' !! 'b')>`, now lower from their existing
`RakuAST::Ternary` shape back to the regex parser. The renderer groups the
condition and both branches so the established match-time subrule-argument
evaluator selects the branch. Consequently the condition keeps observing
lexical reassignment between matches.

No selector or branch is evaluated during `.AST` conversion or RakuAST
lowering, and this adds no matcher or VM path. Modified or quoted method calls
and other dynamic argument expressions remain separate boundaries. The focused
regression is `t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the
ternary node shape, RakuAST EVAL lowering, direct grammar matching, and dynamic
condition reassignment.

## 42. Modified method-call dynamic argument slice (2026-09-15)

Dispatch-modified ordinary method calls in argumented subrules, such as
`<word($value.?uc)>`, now lower from their existing
`ApplyPostfix`/`Call::Method` tree through the shared expression renderer. The
renderer preserves the `.?`, `.+`, and `.*` spelling before the method name,
so the established match-time regex argument evaluator retains dispatch and
lexical lookup semantics.

No method is invoked during `.AST` conversion or RakuAST lowering, and this
adds no matcher or VM path. Quoted method names and other dynamic argument
expressions remain separate boundaries. The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the dispatch field,
RakuAST EVAL lowering, direct grammar matching, and lexical reassignment.

## 43. Quoted method-call dynamic argument slice (2026-09-15)

Quoted method-call expressions in argumented subrules, such as
`<word($value."uc"())>`, now lower from their existing
`ApplyPostfix`/`Call::QuotedMethod` tree through the shared expression renderer.
The renderer preserves the quoted name and explicit empty call, so the existing
match-time regex argument evaluator keeps the quoted-method boundary and its
lexical lookup behavior.

No method is invoked during `.AST` conversion or RakuAST lowering, and this
adds no matcher or VM path. Dynamic quoted names and other dynamic argument
expressions remain separate boundaries. The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the quoted method node,
RakuAST EVAL lowering, direct grammar matching, and lexical reassignment.

## 44. Dynamic quoted method-name argument slice (2026-09-15)

Dynamic quoted method names in argumented subrules, such as
`<word($value."$method"())>`, now retain Rakudo's
`ApplyPostfix`/`Call::QuotedMethod` shape.  The quoted method name remains a
`QuotedString` whose segments contain the lexical expression, rather than
being flattened into a static `Name` or an unquoted dynamic dispatch.

When a constructed RakuAST regex lowers, a single literal name remains the
ordinary quoted `MethodCall`; an interpolated `QuotedString` lowers to the
existing `DynamicMethodCall` execution path.  The regex expression renderer
reconstructs the bounded quoted-string interpolation form, leaving method-name
resolution to the established match-time subrule-argument evaluator.  No
method name is resolved during `.AST` conversion or RakuAST lowering, and this
adds no matcher or VM path.  More complex quoted-string segments and other
dynamic argument expressions remain separate boundaries.

The focused regression is `t/rakuast/rakuast-regex-dynamic-arguments.t`; it
pins the quoted-method and lexical-name tree, RakuAST EVAL lowering, direct
grammar matching, and method-name reassignment between matches.

## 45. Hash-index dynamic argument slice (2026-09-15)

Associative argument expressions written as `%arguments{$key}` now retain
Rakudo's `Postcircumfix::HashIndex` postfix beneath `ApplyPostfix`. The parser
already kept the target, key expression, and associative dispatch bit, and the
existing match-time subrule-argument evaluator already resolved the expression.
The read and write directions now preserve that bit when a regex RakuAST tree
crosses the existing matcher boundary.

This slice is deliberately limited to brace-form hash indexes. The current
internal expression does not retain whether an associative index used `{...}`
or angle-bracket source spelling, so `LiteralHashIndex` remains a separate
source-provenance boundary. No hash lookup occurs during `.AST` conversion or
RakuAST lowering, and no matcher or VM path is added. The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the AST shape, AST EVAL,
grammar matching, and match-time key reassignment.

## 46. Literal hash-index argument slice (2026-09-16)

Associative argument expressions written as `%arguments<primary>` now retain
Rakudo's `Postcircumfix::LiteralHashIndex` postfix beneath `ApplyPostfix`. The
general expression AST intentionally still folds brace and angle associative
subscripts together for execution, so this source distinction is retained only
beside regex subrule arguments. That gives the RakuAST reader the measured
word-quoted key while leaving the existing match-time subrule-argument
evaluator as the sole execution path.

The write direction accepts `LiteralHashIndex`, preserves the angle spelling in
the regex argument source, and lowers its ordinary expression back through the
existing parser, compiler, and VM matcher. No lookup occurs during `.AST`
conversion or RakuAST lowering. The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the AST shape, AST EVAL,
grammar matching, and hash-value reassignment at match time.

## 47. Indirect callable dynamic argument slice (2026-09-16)

Lexical callable invocations in argumented subrules, such as
`<word(&decorate($value))>`, now retain the existing RakuAST
`ApplyPostfix`/`Call::Term` shape and the code-variable target. The parser,
converter, and direct grammar matcher already carried the callable expression;
the missing write-direction boundary was rendering `Expr::CallOn` when a
constructed RakuAST regex returned to the existing regex parser.

The renderer emits the callable target and its argument list without invoking
the callable during `.AST` conversion or RakuAST lowering. The established
match-time argument evaluator therefore remains responsible for callable
lookup and invocation, and no matcher or VM path is added. Named routine calls,
code-bearing arguments, and other runtime-valued forms remain separate
boundaries. The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the code-variable target,
indirect call node, AST EVAL, direct grammar matching, and callable reassignment
between matches.

## 48. Named colonpair dynamic argument slice (2026-09-16)

Named colonpair arguments in argumented subrules, such as
`<word(:expected($value))>`, now retain Rakudo's
`RakuAST::ColonPair::Value` node. The ordinary expression AST intentionally
folds a colonpair value and a bareword `=>` pair into the same `FatArrow`
shape, so the regex argument source records the colonpair delimiter beside
the execution expression for the RakuAST read direction.

The write direction lowers `ColonPair::Value` to the existing named-pair
execution expression and renders its value with the colonpair spelling before
the established match-time subrule argument evaluator sees it. This preserves
named binding and lexical reassignment without evaluating the value during AST
conversion or adding a matcher or VM path. Block-valued and other
runtime-valued colonpair forms remain separate boundaries. The focused
regression is `t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the AST
node and fields, constructor/EVAL lowering, named binding, reassignment, and a
regex-adverb variant.

## 49. Variable colonpair dynamic argument slice (2026-09-16)

Variable colonpair arguments such as `:$expected`, `:@expected`, `:%expected`,
and `:&expected` in argumented subrules now retain Rakudo's
`RakuAST::ColonPair::Variable` node. The ordinary expression AST intentionally
folds these forms into the same named `FatArrow` shape as other colonpairs, so
the regex argument source records the variable-colonpair delimiter beside the
execution expression.

The read direction preserves the key and sigil-specific lexical variable, and
the write direction renders it back as `:{variable}` before the established
match-time subrule argument evaluator sees it. No variable is read during
`.AST` conversion or RakuAST lowering, and no matcher or VM path is added. The
focused regression is `t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins
scalar execution and reassignment plus the array, hash, and callable variable
AST shapes. Boolean and block-valued colonpairs remain separate boundaries.

## 50. Bare boolean colonpair dynamic argument slice (2026-09-16)

Bare boolean colonpair arguments such as `:enabled` in argumented subrules now
retain Rakudo's `RakuAST::ColonPair::True` node. The ordinary expression AST
intentionally folds the colonpair into the same `FatArrow` shape as other
named pairs, so the regex argument source records the bare-colonpair
provenance beside the execution expression.

The read direction preserves the boolean key as a positional
`ColonPair::True` value, and the write direction renders it back as `:enabled`
before the established match-time subrule argument evaluator sees it. No pair
is evaluated during `.AST` conversion or RakuAST lowering, and no matcher or
VM path is added. Negated boolean colonpairs and block-valued colonpairs
remain separate boundaries. The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the source-level node,
constructed AST lowering, and named binding semantics.

## 51. Negated boolean colonpair dynamic argument slice (2026-09-17)

Negated boolean colonpair arguments such as `:!enabled` in argumented subrules
now retain Rakudo's `RakuAST::ColonPair::False` node. The ordinary expression
AST intentionally folds the colonpair into the same `FatArrow` shape as other
named pairs, so the regex argument source records the negated-colonpair
provenance beside the execution expression.

The read direction preserves the boolean key as a positional
`ColonPair::False` value, and the write direction renders it back as
`:!enabled` before the established match-time subrule argument evaluator sees
it. No pair is evaluated during `.AST` conversion or RakuAST lowering, and no
matcher or VM path is added. Block-valued colonpairs and other runtime-valued
regex argument forms remain separate boundaries. The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the source-level node,
constructed AST lowering, and named binding semantics.

## 52. Expression-only block-valued colonpair dynamic argument slice (2026-09-17)

Block-valued colonpair arguments such as
`<word(:expected{ $value })>` now retain Rakudo's
`RakuAST::ColonPair::Value` node with a direct `RakuAST::Block` value. The
ordinary expression AST already preserves the closure as an `AnonSub`; the
regex argument provenance now recognizes the brace delimiter only when that
value is an actual bare block, so hash-composer values are not reclassified.

The write direction renders the supported expression-only block body back as
`:expected{ ... }`, preserving the direct block shape through the shared regex
tree and the existing parser/compiler/VM pipeline. At match time, a block-valued
named argument remains a callable in the callee's code assertion: the matcher
temporarily binds the closure under the callee's named parameter and leaves the
lexical reference in the code block instead of stringifying the Block. This
keeps closure captures live across repeated matches without evaluating user code
during `.AST` conversion or RakuAST lowering.

This slice is deliberately bounded to non-empty, expression-only bare blocks;
placeholder/slurpy block signatures, and other complex runtime-valued forms
remain separate boundaries. The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the direct block node,
constructed AST lowering, named Block binding, and outer-lexical closure across
reassignment.

## 53. Hash-composer block-valued colonpair dynamic argument slice (2026-09-17)

Hash-composer colonpair arguments such as
`<word(:expected{ a => $value, b => 2 })>` now retain the same direct
`RakuAST::Block` value that Rakudo exposes, while preserving the block body's
comma-separated `FatArrow` entries. The parser's execution expression stores
this body as a hash, so the regex argument provenance recognizes the brace form
when the right-hand value is an `Expr::Hash`, in addition to the closure form
handled by the preceding slice.

The write direction flattens the internal `ArrayLiteral` that represents a
RakuAST comma list back into hash-composer entries before sending the source
through the existing regex parser. This keeps the named argument a Hash and
leaves its scalar values dynamic at match time. Empty and simple hash-composer
bodies are covered by the same source boundary; nested or otherwise
unrenderable values, placeholder/slurpy block signatures, and other complex
runtime-valued forms remain deferred.

The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the direct block node,
multiple `FatArrow` entries, constructed-tree EVAL, named Hash binding, and
lexical reassignment between matches.

## 54. Scalar-placeholder block-valued colonpair dynamic argument slice (2026-09-17)

Scalar placeholder block arguments such as
`<word(:expected{ $^candidate eq $value })>` now retain a direct
`RakuAST::Block` value. Each `$^name` in the body is represented as
`RakuAST::VarDeclaration::Placeholder::Positional`, matching Rakudo's
source-level tree instead of exposing the execution AST's synthetic
`PointyBlock` signature.

The lowering direction maps the placeholder declaration back to the
caret-prefixed lexical name consumed by the existing implicit-block closure
builder. Constructed regex trees therefore still lower through the Parser ->
Compiler -> VM path, and the block remains callable with its captured lexical
values at match time. Hash-slurpy placeholders, explicit pointy/signature
forms, and other complex block values remain deferred boundaries.

The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the direct block and
placeholder nodes, source and hand-built regex lowering, named Block binding,
and lexical reassignment between matches.

## 55. Array-slurpy placeholder block-valued colonpair dynamic argument slice (2026-09-18)

Array-slurpy placeholder block arguments such as
`<word(:expected{ @_ })>` now retain Rakudo's direct
`RakuAST::Block` value with a `RakuAST::VarDeclaration::Placeholder::SlurpyArray`
body node. The source-level node is distinct from a pointy block with an
explicit `*@_` signature, even though mutsu's execution AST stores both through
the existing flattened slurpy binding.

The write direction lowers the marker back to `Expr::ArrayVar("_")` and lets
`make_anon_sub` rebuild the implicit `*@_` parameter. Regex colonpair source
rendering therefore keeps the brace form, and constructed RakuAST trees still
use the existing Parser -> Compiler -> VM path with match-time argument and
outer-lexical behavior unchanged. Explicit pointy signatures and other complex
block values remain deferred boundaries.

The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the direct block and
slurpy-placeholder nodes, the no-field constructor, source and hand-built
regex lowering, multiple placeholder arguments, and dynamic outer-lexical
behavior.

## 56. Hash-slurpy placeholder block-valued colonpair dynamic argument slice (2026-09-18)

Hash-slurpy placeholder block arguments such as
`<word(:expected{ %_ })>` now retain Rakudo's direct
`RakuAST::Block` value with a `RakuAST::VarDeclaration::Placeholder::SlurpyHash`
body node. The source-level node is distinct from a pointy block with an
explicit `*%_` signature, while mutsu's execution AST stores the implicit
named slurpy on `AnonSubParams`.

The write direction lowers the marker back to `Expr::HashVar("_")` and lets
`make_anon_sub` rebuild the implicit named slurpy. Regex colonpair source
rendering therefore keeps the brace form, and constructed RakuAST trees still
use the existing Parser -> Compiler -> VM path with match-time named-argument
and outer-lexical behavior unchanged. Explicit pointy signatures and other
complex block values remain deferred boundaries.

The focused regression is
`t/rakuast/rakuast-regex-dynamic-arguments.t`; it pins the direct block and
slurpy-placeholder nodes, the no-field constructor, source and hand-built
regex lowering, named argument binding, and dynamic outer-lexical behavior.

## 57. Explicit pointy-signature block-valued colonpair dynamic argument slice (2026-09-18)

An ordinary explicit pointy block used as a regex colonpair value, such as
`<word(:expected(-> $candidate { $candidate eq $value }))>`, now retains
Rakudo's `RakuAST::ColonPair::Value` with a parenthesized value containing a
`RakuAST::PointyBlock` and its positional `RakuAST::Signature`. The regex
assertion scanner treats the `>` in `->` as part of the nested argument rather
than as the end of the surrounding subrule.

The write direction renders the lowered single-parameter pointy closure back
to the same source form. The existing match-time regex argument evaluator
continues to invoke the closure, so the pointy parameter and captured outer
lexicals remain live. The model constructors needed by a hand-built tree
(`PointyBlock`, `SemiList`, and `Circumfix::Parentheses`) are available on this
bounded path as well. Multi-parameter, typed, defaulted, slurpy, `is rw`, and
statement-rich explicit signatures remain separate boundaries.

The focused regression is `t/rakuast/rakuast-regex-dynamic-arguments.t`; it
pins the colonpair/value/pointy/signature tree, source and hand-built regex
lowering, named callable binding, and lexical reassignment at match time.
