# RakuAST: subscripts, dynamic calls, symbolic names, heredocs

Dynamic method calls, multi-dimensional subscripts, symbolic dereference and a
few smaller forms read back as the nodes rakudo has (measured on rakudo
2026.09), and the lowering rebuilds the parser's own expansion, so the round
trip is the parsed program.

- **Dynamic method calls.** `$o.$name(args)` is an `ApplyPostfix` over
  `Call::TermAsMethod(callee => $name, args)`, `.?` / `.*` its `dispatch`;
  `$o.&f(args)` is a `Call::NameAsMethod(name => f, args)`, but
  `$o.+&f()` (with a dispatch modifier) a `TermAsMethod` over the `&f`
  variable. `@a>>.$name()` / `@a>>.&f` wrap either in `MetaPostfix::Hyper`
  (`src/rakuast/dynamic_method.rs`). A `.^$name()` loses its `^` in rakudo's own
  tree and stays refused.
- **Multi-dimensional subscripts.** `@a[0;1]`, `%h{1;2}` and `@a[*;1]` are the
  ordinary postcircumfix with one `SemiList` statement per dimension; an
  assignment to one is an `ApplyInfix` over `Assignment`. `:exists` and the
  value adverbs (`:kv`, `:p`, `:v`, `:k`) on a multi-dimensional subscript go
  through the same colonpair expansion as a single-dimension one
  (`ast::subscript_adverb` now rebuilds and recognizes the by-name builtin the
  parser makes for them).
- **Zen slices** `@a[]` / `%h{}` are a postcircumfix with no dimension at all.
- **Symbolic dereference.** `$::($n)` / `@::($n)` are a `Var::Package` over a
  dynamic name, `$::($n) = 5` an `ApplyInfix` over `Assignment(:item)` and
  `::($n) = 5` an assignment to a `Term::Name`
  (`src/rakuast/symbolic_deref.rs`).
- **Heredocs.** A `qq:to/END/` body is interpolated through the parser's own
  routine (`parser::interpolate_heredoc_content`) and rendered as the quoted
  string it evaluates to.
- **Hash literals.** `{:x}` keeps its value-less colonpair as `ColonPair::True`.
- `MetaPostfix::Hyper` answers `.postfix`, as in rakudo.

Left for later in the plan (not S7): rakudo's `Heredoc(segments, stop)` node
(the parser drops the terminator, so a heredoc renders as a `QuotedString` —
S9), a heredoc whose marker line closes an enclosing block, the `:delete`
adverb of a multi-dimensional subscript (`__mutsu_multidim_delete*`, built
inline by the parser — S2), a slice or multi-dimensional `:=` bind, and
`@a[0;1]:exists:delete`.

New test: `t/rakuast/rakuast-subscripts-and-dynamic-calls.t` (59 tests), whose
tree part also runs under `raku`. Slice S7 of #7564.
