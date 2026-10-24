# ADR-14845: `.DEPARSE` runs rakudo's own `RakuAST::Deparse`, vendored verbatim

- **Status**: Proposed (2026-10-24)
- **Date**: 2026-10-24
- **Deciders**: tokuhirom, Claude
- **Issue**: [#14845](https://github.com/tokuhirom/mutsu/issues/14845). Roadmap:
  [#7564](https://github.com/tokuhirom/mutsu/issues/7564).
- **Related**: [ADR-10723](10723-rakuast-is-the-frontend-ir.md) (RakuAST is the frontend IR;
  §1.1 names `.DEPARSE` the round-trip contract), [ADR-0096](0096-batteries-adoption-policy.md)
  (run the real upstream code rather than a native substitute),
  [#14844](https://github.com/tokuhirom/mutsu/issues/14844) (the measured RakuAST type hierarchy
  the deparser's multis dispatch on)

## 1. Context

`.DEPARSE` turns a RakuAST tree back into Raku source. It is what the 6.e ecosystem builds on:
the L10N slangs print localized source with `RakuAST::Deparse::L10N::*`,
`RakuAST::Deparse::Highlight` colours source by deparsing it, and Rakuast::RakuDoc::Render and
the documentation toolchain behind it walk and print trees. mutsu has no `.DEPARSE` at all
(measured on `main` `f7a63248`: no file under `src/` mentions it; every call is
`No such method 'DEPARSE'`).

In rakudo 2026.09, `RakuAST::Node.DEPARSE(*@roles)` is

```raku
my $class := my $core := nqp::gethllsym('Raku','DEPARSE');   # RakuAST::Deparse
for @roles { $class := <mix a role in | load a slang by name | take a class> }
$class.deparse(self)
```

and `RakuAST::Deparse` is a 3928-line **Raku** class in the setting
(`src/core.c/RakuAST/Deparse.rakumod`): 267 `multi method deparse` candidates dispatched on the
node types, about 120 one-line methods returning the punctuation and keywords
(`before-comma`, `block-open`, `regex-alternation`, ...) that a subclass overrides, and 172
`nqp::` calls (104 of them `nqp::istype`; the rest string ops: `substr`, `index`, `chars`,
`iscclass`, `eqat`, `findnotcclass`, `x`, `split`). **Subclassing it is the extension point**:
the L10N deparsers override the keyword methods, `Highlight` overrides `hsyn`.

Measured on this branch (after #14844 fixed the hierarchy the multis dispatch on), mutsu
already parses `Deparse.rakumod` unchanged (1.4 s on a debug build), and running it deparses
`RakuAST::IntLiteral.new(3)` to `3`; the first program-sized tree stops on a missing node
accessor (`RakuAST::VarDeclaration::Simple.original-type`).

## 2. Decision

1. **`RakuAST::Deparse` is rakudo's own `Deparse.rakumod`, vendored verbatim** from the rakudo
   release mutsu's RakuAST tracks (2026.09), as a new read-only vendored tree with a
   `vendor.lock` row (`docs/vendoring.md`), never hand-edited.
2. **It is loaded as a core prelude on demand**: compiled once (and kept in the precomp cache)
   the first time a program calls `.DEPARSE` or names `RakuAST::Deparse`, the way
   `Formatter::Syntax` is injected today (`src/runtime/run_prelude_formatter.rs`). A program
   that never deparses pays nothing.
3. **`.DEPARSE` on a node is the rakudo method, routed through Raku code**: the method resolves
   to a prelude routine that does what rakudo's `DEPARSE` does (default class, `@roles` mixed in,
   a `Str` loading `RakuAST::Deparse::L10N::<name>`, a class taking the place of the default)
   and calls `$class.deparse(self)`. The node method reaches it by the ordinary VM call of a
   compiled routine (`vm_call_on_value`), not a runtime slow path.
4. **The interpreter grows until the vendored deparser runs**: every node accessor, `nqp::` op
   and dispatch behaviour it needs is implemented in mutsu itself (the same rule as ADR-0096
   rung 2). A mismatch against rakudo is fixed in mutsu, never by patching the vendored file.
5. **Parity is measured, not asserted**: a corpus script compares mutsu's and rakudo's
   `.DEPARSE` of every top-level statement of every roast file (`.AST` of the file as written),
   and the identical share is the metric, ratcheted like `ci/rakuast-frontend-passing.txt`.

## 3. Consequences

- `.DEPARSE` output matches rakudo by construction once the interpreter runs the deparser, and
  stays matched across rakudo releases by re-vendoring, not by porting diffs.
- Subclassing works as on rakudo, so the L10N and Highlight deparsers (rakudo's
  `lib/RakuAST/Deparse/{L10N,Highlight}/`) can follow as more vendored files under the same
  rule.
- The work the deparser forces — accessors on every node class, `nqp::istype` against RakuAST
  types, multi dispatch on the RakuAST hierarchy — is Raku-level RakuAST parity that any user
  walker needs as well; it is a gain, not a cost of this route.
- A program that deparses pays one prelude compile (a few hundred ms on a release build,
  amortized by precomp) and runs the deparser as bytecode, slower than a native printer would be.
  That is a measured cost to optimize transparently if it matters (ADR-0096 §D3), not a reason to
  replace it.
- One more vendored upstream tree to keep in step: `scripts/update-vendor.sh` has to learn a
  sparse checkout of the rakudo files.

## 4. Alternatives

- **A Rust port of the deparser over `RakuAstNode`.** Fast and self-contained, but a second copy
  of 3.9k lines that drifts from rakudo on every release, and a Raku subclass cannot override
  a Rust method, so the L10N and Highlight deparsers would still not work. Rejected.
- **A hand-written, smaller Raku deparser.** Same drift, and no reason to expect it to match
  rakudo's whitespace and parenthesization choices, which is what `.DEPARSE`'s consumers compare.
  Rejected.
- **A bundled battery (`use RakuAST::Deparse`).** `.DEPARSE` needs no `use` in rakudo; it is
  core. Rejected.

## 5. Open questions

- Where the vendored rakudo files live (`vendor/rakudo/` with a sparse checkout of
  `src/core.c/RakuAST/Deparse.rakumod`, later `lib/RakuAST/Deparse/`) and how
  `update-vendor.sh` names the tree.
- Whether the precomp cache keys the prelude by the vendored commit (it should, so a re-vendor
  invalidates it).
