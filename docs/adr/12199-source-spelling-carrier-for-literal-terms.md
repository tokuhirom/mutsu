# ADR-12199: Word lists, heredocs and bare-statement prefixes keep their source spelling in a wrapper the compiler never sees

- **Status**: Accepted (2026-10-07). Word lists implemented (section 6); heredocs and the bare-statement prefixes are not.
- **Date**: 2026-10-07
- **Deciders**: tokuhirom, Claude
- **Issue**: [#12199](https://github.com/tokuhirom/mutsu/issues/12199). Roadmap:
  [#7564](https://github.com/tokuhirom/mutsu/issues/7564) (slice S9, PR #12189)
- **Related**: [ADR-10723](10723-rakuast-is-the-frontend-ir.md) (the parser will emit RakuAST;
  this ADR is the bridge until it does), [ADR-10499](10499-mutable-typed-ast-visitor.md) (`VisitMut`,
  the rewriting visitor the strip pass uses), [ADR-0137](0137-typed-ast-visitor-for-analyses.md)

## 1. Context

S9 made the `.AST` text of supported constructs equal rakudo's by recording how the parser
*spelled* a construct in a field the compiler ignores (`Expr::Call.listop`, `Binary.form`,
`MethodCall.sugar`, `Index.spelling`, `IndexAssign.spelling`, `CallArg::Named.form`,
`Unary.word`). On the corpus (`scripts/ast-text-corpus.sh`, 10995 statements) 91.1% of the
statements are now identical to rakudo's; the largest classes that remain are:

| class | statements | rakudo's node | what the parser keeps |
| --- | ---: | --- | --- |
| word list `<a b  c>` | 259 (2.4%) | `QuotedString(processors => <words val>, segments => ("a b  c",))` | `ArrayLiteral` of three `Literal`s (a single word: one `Literal`); the raw text and its whitespace are gone |
| heredoc | 53 (0.5%) | `Heredoc(segments, stop => "    END\n")` | a `Literal(Str)` / `StringInterpolation` after dedent; the terminator line is gone |
| bare-statement prefix (`gather STATEMENT`, `start STATEMENT`, `BEGIN STATEMENT`) | about 30 | the prefix over a `Statement::Expression` | the same block `gather { STATEMENT }` makes |

A field does not fit these: the values are `Expr::Literal` (1032 match sites in `src/`),
`Expr::ArrayLiteral` (314: 197 in the parser, 69 in the compiler, 29 in `rakuast`) and
`Expr::StringInterpolation`, and the spelling has no home on them. The question is where the
spelling lives, and it has to be answered without changing what a program does: `my @a = <a b>`,
`for <a b> { }`, the constant folders and the parser's own shape checks all match on the
`ArrayLiteral` shape.

Precedents in the tree: `Expr::LiteralSrc(Value, Box<str>)` (a literal that remembers its source,
transparent to the compiler; 45 sites had to learn it) and `Expr::Grouped` (a parenthesised
expression the compiler compiles through; 112 sites).

## 2. Options

**A. A wrapper the compiler never sees.** `Expr::Spelled(Box<Spelled>)` with
`Spelled { expr: Expr, spelling: Spelling }` (`Spelling` = word list text and its quote kind,
heredoc terminator, bare-statement prefix). The parser wraps the construct it built; a single
`VisitMut` pass (`strip_spelling`) replaces every wrapper by its `expr` before the tree reaches
the compiler, the precompilation cache or any analysis. `.AST` takes the tree *before* the strip
and reads the spelling. Cost: one variant, one pass, two parse entry points.

**B. More transparent variants, like `LiteralSrc` and `Grouped`.** `Expr::WordList { items, source }`
compiled as `ArrayLiteral`. Cost: every site that matches `ArrayLiteral` (314) must also learn the
new variant, and one that does not changes behaviour silently (a `for <a b>` that stops being
recognised as a literal list). The 45- and 112-site footprints of the two precedents are the
measure.

**C. A side table.** The parser records `(position, spelling)` and the converter looks the spelling
up. `Expr` has no positions and no ids; matching by the ordinal of the n-th word list in a
statement breaks as soon as a pass duplicates, drops or reorders a node (the `wrap_composition_operands`
bug of S9, where a rebuild silently dropped every form, is the failure mode).

**D. Wait for ADR-10723 Stage 2.** The parser emits `QuotedString` / `Heredoc` itself and `lower`
chooses the compiler's form, so no carrier is needed. The classes stay at about 3% of the corpus
until the quoting family is moved, and Stage 2 has no date.

## 3. Decision (proposed): A, designed to be deleted by Stage 2

1. **The parser attaches the wrapper at the term**, in `angle_list` / `french_quote_list` /
   `double_angle_list`, the heredoc parsers and the `gather` / `start` / phaser statement-prefix
   parsers. `Spelled` is built last, so the code that built the inner `Expr` is unchanged.
2. **`parse_program` strips; `.AST` does not.** `strip_spelling` runs at the end of the
   executing entry points (`parse_program`, `EVAL`, module loading), so the compiler, the
   precompilation cache and every analysis see exactly the tree they see today. The `.AST` entry
   points (`Str.AST`, `MUTSU_RAKUAST=1`) call the variant that keeps the wrapper; `convert` renders
   the RakuAST node from `Spelled`, and `lower` produces the wrapper for a `QuotedString` /
   `Heredoc`, so the round trip keeps its spelling.
3. **Parse-time consumers.** The parser's own shape checks (197 `ArrayLiteral` sites) run before
   the strip. A term parser that is followed by a shape check peels the wrapper through
   `Expr::peel()` at that site; the sites to touch are found by the experiment in §4, not by
   guessing, and a shape check that is missed is caught as a behaviour difference (§4).
4. **No second representation.** `Spelling` holds only what RakuAST needs and the `Expr` does not
   have (text, terminator); the value stays in `expr`, so the two cannot disagree.
5. **Deleted by Stage 2.** When the parser emits RakuAST for the quoting family, `Spelled` and the
   strip pass go with the converter arms they feed; the ADR-10723 stage plan records it.

## 4. How to find out whether A holds (the first PR)

Implement the word list only (the biggest class, 2.4%) and gate on all of:

- the `MUTSU_RAKUAST=1` ratchet (`scripts/rakuast-frontend.sh check`) and `make test`, `make roast`
  unchanged (the strip makes the executing tree identical, so any difference is a missed `peel`);
- the corpus class `words-quote` at zero on the same sample, and `t/rakuast/rakuast-source-forms.t`
  gaining `<a b  c>` with its double space;
- parse plus strip cost within noise on the compile-heavy benchmarks (`docs/benchmarks.md`): the
  pass is O(n) over a tree the parser just built, and is skipped for a program with no wrapper
  (the parser counts them).

Abort criterion: a behaviour change that needs more than a handful of `peel` sites, or a measured
parse cost above 1%. Then B or D is chosen instead, with the numbers recorded here.

Heredocs and the bare-statement prefixes follow in separate PRs once the word list has shown the
mechanism holds.

## 5. Consequences

- The compiler, the VM and the precompilation cache are untouched.
- `.AST` of a word list, heredoc or `gather STATEMENT` can equal rakudo's, closing the three
  classes of #12199 (about 3% of the corpus).
- A new variant exists for the length of the hybrid period; a pass that rebuilds a node must carry
  it (the same rule `docs/rakuast/README.md` states for the S9 fields).
- If Stage 2 lands first for these constructs, this ADR is superseded without any code to remove.

## 6. Implementation status

### 6.1 Word lists (2026-10-07, [#12199](https://github.com/tokuhirom/mutsu/issues/12199))

The experiment of section 4 held; option A is kept with one refinement and no abort criterion
was met.

**Refinement: the wrapper is built only when the parse asks for it, so there is no strip pass.**
Section 3.2 had every executing entry point strip the wrapper again. The `Expr`-returning entry
points the compiler calls lazily (`interpolate_qq_content`, `parse_qq_interpolation`) and the
nested parses a module load starts made "strip at the end of the executing entry points" a list
that had to be kept complete by hand. Instead `ast::spelled::keep_spelling(true)` is set by the
parses whose tree only the RakuAST conversion consumes (`Str.AST` through
`parse_dispatch::parse_source_spelled`, and the units `MUTSU_RAKUAST` round-trips,
`rakuast::frontend::covers`); every other parse leaves it off, so `Expr::spelled` returns the plain
expression and the compiler, the precompilation cache and the analyses see exactly the tree they
always saw. A nested parse sets its own value and puts the outer one back (`KeepGuard`). No tree
that reaches the compiler can contain the wrapper by construction, which a strip pass can only
promise by being complete; the compiler arm stays only as a `debug_assert!` backstop. The cost
on the executing path is one thread-local read per `<...>` term (no measurement was taken: there
is no pass to measure).

**What was needed.**

- `Expr::Spelled(Box<Spelled>)`, `Spelled { expr, spelling }`, `Spelling::Words(raw text)` in
  `src/ast/spelled.rs`. The four exhaustive matches over `Expr` (`walk_expr`, `walk_expr_mut`,
  the whatever-curry marker, the compiler) got an arm each; no other site failed to compile.
- `parser::primary::container::angle_term` wraps the `<...>` term. `angle_list` stays unwrapped:
  the declarator trait-argument sugar (`is assoc<left>`) reads its result directly and RakuAST
  has its own node for it. A single numeric literal (`<1/2>`, `<1+2i>`) is a number term in
  rakudo, not a quote, and is not wrapped.
- `Expr::peel_parens` also looks through `Spelled`. **One parse-time shape check needed a peel**:
  `handle_specs_from_term` (the `handles <a b>` clause of an attribute) matched the term after
  peeling only `Grouped`, so under a spelling-keeping parse it read no specs and the converter
  refused the attribute. It was found by the `MUTSU_RAKUAST=1` ratchet (26 of the 5979 listed
  files failed), not by the probes or the corpus, which is why the ratchet is a gate of this
  step. The abort criterion ("more than a handful of `peel` sites") was not approached; the
  shapes probed against rakudo (`for <a b>`, `use Test <plan>`, `my ($x, $y) = <a b>`,
  `<a b>, <c d>`, `<a b> Z <c d>`, postfix calls and subscripts) and the corpus needed none.
- `convert` renders `Spelled(Words(text))` as `QuotedString(processors => <words val>, segments
  => (StrLiteral(text),))`. `lower` already turned that node into the compiler's form
  (`angle_words_expr`), so the round trip needed nothing.

**Measured.** On the corpus (`scripts/ast-text-corpus.sh`, 763 files, 11779 statements):
94.7% of the statements are identical to rakudo's, and the `words-quote` class is at zero. The
class used to claim every statement whose rakudo text contains `words val`, which includes a
declared operator name (`sub infix:<foo>` is `Name.from-identifier("infix", colonpairs =>
(QuotedString words))`); those 54 statements belong to the existing `name-parts-colonpairs` class
and the rule now says so. No statement mutsu renders with a word quote differs from rakudo's
(0 of 510 other-class hunks). All 5979 files of the `MUTSU_RAKUAST=1` ratchet pass.

**Other word quotes (2026-10-07, [#12228](https://github.com/tokuhirom/mutsu/issues/12228)).**
`Spelling::WordQuote { quotewords, val, text }` carries the processors of `qw`/`Qw`/`q:w`
(`words`), `qww`/`qqww`/`qq:ww` (`quotewords`) and `«…»`/`<<…>>` (`quotewords val`); the quote
parsers (`spell_word_quote`, `french_quote_term`, `double_angle_term`) wrap only a quote whose
text is plain (`word_quote_text_is_plain`: no interpolation, inner quote, nesting or escape).
`french_quote_list`/`double_angle_list` stay unwrapped for the subscript and colonpair callers,
as `angle_list` does. The lowering of every processor list is `parser::word_quote_expr`. The
interpolating forms (`<<a $b>>`, `«a "b c"»`) need segment nodes and remain refused.

**What remains** is the closed slice list in
[#12199](https://github.com/tokuhirom/mutsu/issues/12199) (measured 2026-10-07 on a 766-file,
11827-statement sample, 94.6% identical): S2 heredocs (46 statements), S3 the bare-statement
prefixes at expression level (`try`, `gather`, `start`, `once`, `do`; 52 occurrences) and S4 the
statement-level ones (`Stmt::Phaser`, `FIRST`/`NEXT`/`LAST STATEMENT`; 19 occurrences), one PR
each, each adding a subsection here with its measured result, its peel sites and its carrier
decision. The list does not grow: a finding that is not "the parser discards how this was
spelled" is its own issue. Two such findings exist: the word quotes other than `<...>` (`qw`,
`«»`, `<<>>`, `qqww`, `q:w`), which `.AST` refuses today
([#12228](https://github.com/tokuhirom/mutsu/issues/12228)), and operator names with a colonpair
(`sub infix:<foo>`, [#12220](https://github.com/tokuhirom/mutsu/issues/12220)).

### 6.2 Heredocs (2026-10-07, [#12199](https://github.com/tokuhirom/mutsu/issues/12199), slice S2)

Option A held again; no abort criterion was met.

- **Carrier**: `Spelling::Heredoc { stop }` on `Expr::Spelled`, built by
  `parse_to_heredoc_with_flags` (`src/parser/primary/string/heredoc.rs`) through
  `Expr::spelled`, so only a spelling-keeping parse sees it. `stop` is the terminator line as
  written: its indentation, the delimiter and the newline that ends it (none at the end of the
  source). The `:w` adverb is not wrapped: rakudo keeps it as a `processors => ("words",)` field
  over the unsplit text, which the parser has already split (a separate finding, not a slice).
- **Peel sites: none.** The parse-time consumers of the heredoc term (`check_heredoc_scope_errors`,
  the `closes_block_same_line` flag) run on `Expr::HeredocInterpolation`, which stays the wrapped
  expression; nothing needed `peel_parens`.
- **Read**: `convert` renders the wrapped expression the way it renders any quoted text and turns
  the resulting `QuotedString` into `RakuAST::Heredoc` with a `stop` field, so `qq` interpolation
  parts (variables, blocks) come out as rakudo's segments. **Write**: `RakuAST::Heredoc.new(
  segments => ..., stop => ...)` is a constructor, and `lower` treats it as the `QuotedString` it
  wraps (`stop` has no effect on the lowered string).
- **Measured**: on the corpus (758 files, 11428 statements) the `heredoc` and `heredoc-stop`
  classes are at zero and no class grew; 94.6% of the statements are identical. The
  `MUTSU_RAKUAST=1` ratchet passes for all listed files (one file, `attribute-lazy-initialization.t`,
  timed out under 4-way parallel load on a debug build and passes alone).

### 6.3 Bare-statement prefixes, expression level (2026-10-07, [#12199](https://github.com/tokuhirom/mutsu/issues/12199), slice S3)

Option A held a third time; no abort criterion was met. S4 (statement level) is still to do.

- **Carrier**: `Spelling::BareStatement` on `Expr::Spelled`, built by the parsers of `try STMT`,
  `gather STMT`, `start STMT`, `once STMT`, `BEGIN`/`CHECK`/... `STMT` and `do STMT`
  (`identifier_call.rs`, `term_literals.rs`) through `Expr::spelled`, around the same prefix
  expression the braced form builds. The wrapped expression is unchanged, so only a
  spelling-keeping parse sees the marker.
- **Peel sites: none found by the ratchet.** One latent site exists and is left alone:
  `lvalue_assign_to_expr` matches `Expr::Try` to move `(try LVALUE) = RHS` inside the prefix; under a
  keeping parse a bare `try` is wrapped and takes the generic path. It affects only the tree shape
  of that rare form, not execution.
- **Read** (`src/rakuast/bare_prefix.rs`): the prefix is converted as usual, then its one-statement
  `Block` child is replaced by the statement. `do STATEMENT` becomes `StatementPrefix::Do` over
  any converted statement (the plain converter only accepted loops and conditionals, so the corpus
  skipped files containing it).
- **Write**: `lower` takes either a `Block` or a statement as the prefix's child
  (`bare_prefix::lower_body`; `start` lowers any non-block child through `lower_stmt`, which also
  made `start react { ... }` and `start until ... { ... }` round-trip). The constructors
  `StatementPrefix::{Do,Try,Gather}.new` were missing from the class table and are added.
- **Measured**: corpus (760 files, 11624 statements) 95.1% identical (94.6% before); the new
  `bare-prefix` class is at zero. Two hunks of the old `statement-prefix` class remain and are not
  spelling findings: `try X if C` (modifier scope) and `sink A, B` ([#12240](https://github.com/tokuhirom/mutsu/issues/12240)).
  The `MUTSU_RAKUAST=1` ratchet failed on two files at first (`start react`/`start until`, fixed
  above); the three timeouts left in a 4-way parallel debug run pass alone.

### 6.4 Bare-statement prefixes, statement level (2026-10-07, [#12199](https://github.com/tokuhirom/mutsu/issues/12199), slice S4)

Option A held a fourth time; no abort criterion was met. This closes the slice list.

- **Carrier decision**: `Stmt` has no wrapper variant, and `Stmt::Phaser` is built and matched in
  dozens of places, so neither a wrapper variant nor a new field was worth its peel sites. A
  spelling-keeping parse of a phaser over a bare statement (`LEAVE say 1`, `FIRST say 1`) returns
  `Stmt::SyntheticBlock([Stmt::SourceForm(SourceForm::BarePhaser), Stmt::Phaser { .. }])`, the
  existing source-form record pattern (`SignatureDecl`, `SupplyBlock`). The phaser inside is
  unchanged, and no executing parse builds the record (`ast::spelled::keeping()`).
- **Peel sites: none found.** The `MUTSU_RAKUAST=1` ratchet failed on one file
  (`begin-selective-import-proto-multi.t`), which was a bug in the converter's unwrapping of the
  `FIRST`/`NEXT`/`LAST` synthetic block (it must not touch other kinds), not a parse-time consumer.
- **Read** (`bare_prefix::bare_phaser_statement`): the phaser is converted as usual and its block
  child replaced by the statement. `FIRST`/`NEXT`/`LAST` keep their bare statement in a
  scope-less `SyntheticBlock`, which is unwrapped first. **Write**: `lower_phaser` takes a block or a
  bare statement (`bare_prefix::lower_phaser_body`), re-wrapping the loop phasers' statement.
- **Measured**: the `bare-prefix` class is at zero on the corpus (1426 files, 21784 statements,
  95.0% identical); the `MUTSU_RAKUAST=1` ratchet passes except three debug-build timeouts that
  pass alone.
