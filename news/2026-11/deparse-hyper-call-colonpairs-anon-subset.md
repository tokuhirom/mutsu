# `.DEPARSE`: `».(args)`, `:a:!b:42c`, `subset ::` and raw declarator comments

More `.DEPARSE` differences against rakudo 2026.11 closed (#14845):

- `@a».(args)`, which runs as `@a».CALL-ME(args)`, is
  `MetaPostfix::Hyper(Call::Term(args))` and deparses as `@a>>(args)`.
- Colonpairs written one after another as a term (`my @x = :a:!b:42c`,
  `my $c = :a :b`) are the new `RakuAST::ColonPairs` node and deparse as
  written; a statement made of colonpairs stays a comma list, as in rakudo.
- An anonymous `subset :: where ...` keeps its `::` name
  (`Name(EmptyEdge, EmptyEdge)`) instead of the parser's internal
  `__mutsu_anon_subset_N`; lowering mints a fresh internal name for it.
- A declarator comment (`#=` / `#|`, bracketed or not) keeps its text as
  written in RakuAST (`#={\nsimple case\n}` is `trailing => ("\nsimple case\n",)`,
  `#=   foo` keeps its spaces), so `.DEPARSE` reproduces it. `.WHY` answers each
  line trimmed and joined by a space, as rakudo does: `#={  a\n  b   c }` is now
  `"a b   c"` (it collapsed every run of whitespace).
