# A nested-name coercion target is found inside its own package

`sub select(Column::List(Any) $columns)` in a module that declares `class Column::List` died with
"Impossible coercion from 'Str' into 'Column::List'": the coercion looked up `COERCE` on the short
name, which is not what the class registers under (`Pk::Column::List`). The target is now resolved
against the running package the way a bareword type is. SQL::Abstract's first `select` gets past
this; its `has Window::Clauses $.windows` attribute check has the same short-name gap and is filed
separately.
