# One mechanism for "a block's `}` ends the statement", one `use lib` decoder, one chain flattener

A survey of the hand-rolled AST walkers left by #10468 turned up three families
of duplicated shape logic. Each now has a single implementation.

**Statement-ending braces.** Raku ends a statement at a block's `}` followed by
a newline, so a next-line `if`/`unless`/`for` begins a new statement rather than
modifying the previous one. mutsu decided this by re-deriving "does this
statement end in a block?" from the AST, in two diverging copies
(`parser/stmt/modifier.rs` and `parser/stmt/control/for_loops.rs`), plus a third
`do`-only copy in the destructuring-declaration parser. Each copy handled
variants the others missed, so these all died with "Confused. Two terms in a
row" while rakudo runs them:

```raku
my $h = {a => 1}
if False { say "then" }

my @a = 1, -> { 2 }
if False { say "then" }

my $x = 1 + do { 2 }
if False { say "then" }
```

The parser already recorded, for the infix layers, the position of the first
token after a block's `}` + newline (`parser::stmt_ending_brace`, rakudo's
`$*ENDSTMT`). The statement-modifier parser now asks that same record instead
of inspecting the AST, and the hash-composer and regex-declaration braces now
set it too, as rakudo's blockoids do. The AST classifiers are gone. The parse
memo now replays these marks on a hit, so a memoized expression leaves the same
marks a fresh parse would.

The `for` parser's "Expression needs parens to avoid gobbling block" check
(rakudo's `$*BORG<block>`) uses a sibling record of the last block *term*
(bare block, pointy block, hash composer). It now matches rakudo for
`for (1, {2})`, `for 1, {2}, 3` and a trailing comment after the gobbled block,
and no longer reports a gobble for `for 1, sub { 3 }`.

**`use lib` arguments.** The parser, the compiler's nested-`use` prologue and
the runtime's pre-execution type scan each decoded a `use lib` argument list
themselves; the parser's copy unpacked one bare list level and did not look
through parentheses. They now share `use_lib_args`, keeping their own leaf
rule, and the path-chain folder also looks through a parenthesised link
(`use lib ($?FILE.IO.parent).add('lib')`).

**Operator chains.** Two byte-identical `^^` chain flatteners, the compiler's
`X`/`Z` chain collector, the parser's multi-way-zip collector and RakuAST's
list-infix flattener are now `Expr::flatten_binary_chain` and
`Expr::flatten_meta_chain` (`src/ast/chains.rs`).

The hand-rolled walker ratchet (`scripts/ast-walkers-baseline.txt`) fell by ten
across nine files.
