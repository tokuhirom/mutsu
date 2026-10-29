# The attribute compound-initializer lowering lives in `rakuast`

ADR-15334 (reading B), `has` family. `has $.g //= EXPR` (any compound operator after an attribute
declaration, public, private or alias, typed or not) is now the one typed `ParsedStmt::DeclCompound` node the
parser builds for `my $x OP= EXPR`, and `rakuast::decl_compound_assign` is its only lowering. The parser used
to desugar the public form (and every form followed by a postfix modifier) into a `SyntheticBlock` of the
declaration and an assignment itself; that producer and `src/parser/stmt/decl/has_compound_assign.rs` are gone,
along with the `parser::attribute_compound_assign` / `expand_attribute_compound_assign` re-exports. With a
postfix modifier (`has $!h ||= 5 if COND`) the `ParsedStmt::AttributeModifier` node now wraps the compound
node, and its lowering (and `modifier_lower`, for the `.AST` round trip) keeps the declaration, its assignment
and the runtime-only condition in one flat block.

Side effects of reading the form once: the public and modified forms now round trip through `.AST` (they were
refused with "does not yet support this construct"), and a postfix modifier after a compound initializer is
recognized after the whitespace like it is after `=`.

`t/rakuast/rakuast-stage2-attribute-compound-paths.t` pins that a file, an `EVAL` string and an `EVAL` of the
string's `.AST` declare the same attributes, plus the `ApplyInfix` / `MetaInfix::Assign` read direction; it
passes identically under `raku`. The `synthetic-block-sites` ceiling drops 47 to 44.
