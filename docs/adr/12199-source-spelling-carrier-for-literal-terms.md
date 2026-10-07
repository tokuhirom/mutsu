# ADR-12199: Word lists, heredocs and bare-statement prefixes keep their source spelling in a wrapper the compiler never sees

- **Status**: Accepted (2026-10-07). Not implemented: the first PR is the word-list experiment of section 4.
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
