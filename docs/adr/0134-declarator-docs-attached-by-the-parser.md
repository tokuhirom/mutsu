# ADR-0134: Declarator docs are attached by the parser, not by a source line scanner

- **Status**: Accepted (implemented)
- **Date**: 2026-09-30
- **Related**: [#10226](https://github.com/tokuhirom/mutsu/issues/10226), roast `dd85d3c9`
  (S26 leading docs moved into initializers), rakudo `1f48abb1d` (RakuAST sets declarator
  docs at BEGIN time)

## Context

`Interpreter::collect_doc_comments` (`src/runtime/io_doc.rs`, ~1340 lines) attached `#|` / `#=`
declarator docs by re-scanning the **source text line by line**: string heuristics recognized a
declaration on a line (`try_extract_declarant`), brace counting tracked class scopes, and
per-kind keys were synthesized from the text (`&<anon>.N`, `block:$var`, `Class::method/multi.N`).
`.WHY` then found a doc by those keys — an anonymous sub by counting `&<anon>` lines and, failing
that, by *source-line proximity*. The parser skipped `#|` as whitespace.

Every new layout of a doc comment needed another heuristic. When roast `dd85d3c9` moved three
leading docs into initializers (`my $anon-sub = #| Anonymous` + `anon Str sub {};`), the fix was
yet another text rewrite (`io_doc_hoist.rs`). And the scanner gave a wrong answer the heuristics
could not express: `#| doc` above `my $x = anon sub {}` documented the sub, while Rakudo attaches
it to the variable — the declaration right after it.

## Decision

1. **The parser decides what a doc comment documents** (`src/parser/decl_doc.rs`):
   - `ws` reports every declarator comment it skips; a `#|` remembers the offset of the token
     after it. A declarator comment needs horizontal whitespace or an opening bracket right after
     the marker, as in Rakudo, so `#=====` and `#|x` are plain comments.
   - Each declaration records its extent when its parser succeeds: statements (routines,
     methods, packages, grammar rules, attributes, enums, subsets, variables), signature
     parameters, and anonymous routines/blocks used as terms. `unit` packages extend to the end
     of the source.
   - When the unit is parsed, a `#|` goes to the **next declaration to start after it**,
     wherever that is (this is Rakudo's rule: `#| doc\nsay 1;\nsub f {}` documents `f`, and
     `#| doc\n is sub {}, ...` the sub). A `#=` goes to the declaration that **started last
     before it among those still claiming it**: its extent, plus the whitespace after it past one
     `;`, `,` or invocant `:` (a block claims only its own extent, so the `#=` after
     `subset S where { ... };` documents the subset).
   - Variables take their comments too — so `#|` above `my $x = anon sub {}` no longer reaches
     the sub — but nothing reads a variable's documentation yet (roast still skips
     "declaration comments are NYI on variables").
2. **All positions are source offsets**, so backtracking and memoization cannot disturb the
   result: a recorded comment or extent is a fact about the source text, recorded identically by
   every parse that reaches it. Code the parser reads from a heredoc's leaked copy of the rest of
   the source maps back through the new `source_offset`.
3. **What the parser produces**:
   - For a named declaration, a `DocComment` (`src/decl_doc.rs`) in source order, named by the
     structure the parse recorded — enclosing packages, multi candidate index, role variant,
     owning routine of a parameter. The keys are the ones `.WHY` and the `$=pod` builder's
     concrete declarants already use; they are now derived from parse structure, not from text.
   - For an anonymous routine or block, a `DocSlot` on the AST node itself. The node is built
     (and memoized, and cloned) before the unit is parsed to the end, so the parser gives it an
     empty slot and fills it once in `finish_unit`; clones share the slot. The compiler copies
     it onto the closure's `CompiledCode::declarator_doc`, and `.WHY` on the closure reads it
     there — no counter, no line proximity.
4. **The runtime installs, it does not scan.** `parser::decl_doc::take_unit_docs` hands the
   unit's docs to `establish_pod_variables_from_stmts` / `add_pod_declarator_entries_from_stmts`
   (mainline, module, EVAL, `--doc`). A module's docs are a `ParseEffects` entry, replayed on a
   precompilation-cache hit like the other parser effects. A nested best-effort parse
   (`parse_program_recovering`) records nothing into the enclosing unit's table, and a fragment
   parse does not replace the enclosing unit's published docs.

## Consequences

- `collect_doc_comments`, its second "source order" re-scan, the `&<anon>` key, the
  source-line proximity match and the `io_doc_hoist.rs` rewrite are gone.
- The leading-doc semantics now follow Rakudo main: the three roast files `dd85d3c9` rewrote pass
  in their new shape, and their old shape (`#|` above `my $x = anon Str sub {}`) no longer
  documents the sub.
- Named declarations are still found by structural key at `.WHY` time. Moving their association
  onto the declared objects themselves (as for anonymous code) would drop the key synthesis in
  `dispatch_why` too; it is not needed for correctness now and is left for when those objects
  carry a stable declaration identity.

## Rejected alternatives

- **Keep the scanner, add heuristics** (what `io_doc_hoist.rs` did): each layout is another
  special case, and the variable case cannot be expressed on lines.
- **Mutable parser state ("pending doc", "preceding declaration")**: the parser backtracks and
  memoizes; state consumed by a speculative parse is lost or double-counted. Offsets are
  idempotent.
- **A `doc` field on every declarator AST node**: ~12 node kinds and hundreds of construction
  sites, for information the runtime reads by name anyway. Only anonymous code, which has no
  name, carries its documentation on the node.
