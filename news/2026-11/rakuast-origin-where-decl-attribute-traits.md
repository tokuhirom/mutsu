# RakuAST: where-clause visit order, attribute traits beside a default

Three `.origin` / `visit-children` gaps closed (#15780):

- a declaration with a `where` clause (`my Int $x where * > 1 = 3`) now
  visits as Rakudo does: the clause, the type, the initializer, the type
  again, then its traits (`visit_order::where_declaration`);
- an attribute's traits are placed beside its default
  (`has $.x is rw = 1`), whose implicit `Trait::WillBuild` is not written in
  the source and no longer throws off the trait count;
- an empty substitution replacement (`s/\d+//`) places its `QuotedString`
  and empty `StrLiteral` where the replacement would be written.
