# A trailing comma before a statement modifier parses again

`die "x", if @c;` is legal Raku — the trailing comma is an empty list slot, not a
syntax error — but mutsu rejected it with `expected statement: expected '.' or
digits or generic radix literal or ...`. The same applied to `return 1, if 0;`
and to a paren-less listop argument list (`die sprintf '...', $a, $b, if ...;`).

Found by re-running the real-dist compatibility sweep (PLAN §B4): it is what
blocked the whole `UpRooted` distribution, which writes exactly that shape in
`UpRooted::Table`:

```raku
die sprintf 'UpRooted::Column %s has order conflict in UpRooted::Table %s.', $column.name, $.name,
    if %!columns.values.grep: *.order == $column.order;
```

## Root cause

Four different argument-list parsers each decide when a comma was a *trailing*
comma. All of them treated only `;`, `}`, `)` and end-of-input as terminators, so
after consuming the comma they tried to parse the modifier keyword as a term and
failed. `listop.rs` (the general user-sub listop path) already had the
statement-modifier check; the other three did not:

- `stmt/modifier.rs::parse_statement_modifier` — for statements that parse a
  single argument expression (`die`/`fail`, via `expression_no_word_logical`,
  which does not consume commas at all, so the comma was still pending).
- `stmt/assign/comma.rs` — the comma-list parser used by `return` and the
  assignment/declaration RHS. Both trailing-comma checks now go through one
  `comma_list_ends_here` helper.
- `primary/ident/identifier_call.rs` — twice: the builtin-listop argument loop
  and the general no-paren-call loop. This is why `sprintf`/`join` heads failed
  while a user sub of the same shape already worked.

The new predicate `is_stmt_modifier_after_trailing_comma` deliberately rejects a
same-named **pair key**: `keyword()` only checks a word boundary, so a bare
`is_stmt_modifier_keyword` would have matched the `with` in `my @a = 1, with =>
2` and silently truncated the list at its last element. The predicate therefore
requires that the keyword is not followed by `=>`.

## Result

All 24 `UpRooted` modules now load (the two remaining failures are its missing
`DBIish` dependency, not a mutsu bug); before this it was one of the two
`parse_error` rows in the sweep.

The same sweep re-run also confirmed the earlier "~half of dep-satisfiable dists
hit a real mutsu bug on `use` alone" figure is out of date: of 60 sampled dists
(seed 20260719) 19 load, 20 are `missing_dep`, and only 6 are real mutsu
failures (2 `parse_error`, 3 `runtime_error`, 1 `timeout`).

Pinned by `t/trailing-comma-before-statement-modifier.t` (12 subtests covering
`die`/`fail`, `return` with true and false modifiers, a multi-element `return`,
both builtin and general listop heads, the pair-key guard, and an ordinary
trailing comma). All 12 identical under raku.
