# `Any`'s iteration methods are rows of the method table

`map`, `grep`, `first`, `reduce`, `produce`, `rotor`, `skip`, `squish`, `eager`, `iterator`, `match`, `classify` and `categorize` on `Any`, and
`classify-list`/`categorize-list` on `Hash`, are now `Handler::Interp` rows of the one method table (ADR-11276 §9.60, issue #12388). Each row
calls the same interpreter routine the dispatch cascade's arm calls, after declining the receivers the cascade deferred to the interpreter
(a user `iterator`, a lazy pipe source). The methods answer as before; the table now lists them, so the resolver can reach them as native
candidates.
