# `temp $.attr ~= …` and a second quantifier on a regex alias

Two gaps surfaced by the CSS::Writer distribution's own test suite:

- `temp` on a method lvalue only accepted the plain `temp $obj.meth = value`
  form. `temp $.indent ~= '  '` (the `$.attr` spelling of `self.indent`),
  `temp self.n = 7` and the compound `temp $obj.attr ~= value` all fell
  through to a call of an unknown function `temp`. They now temporize the
  invocant exactly like the plain form and restore it at scope exit.
- A regex sigil alias binds one *quantified* atom, so a second quantifier
  quantifies the whole alias: `/^ $<w>=.*? +%% X $/` is `[$<w>=.*?]+ %% X`,
  with `$<w>` a List of per-iteration Matches. mutsu rejected it with
  "Quantifier quantifies nothing"; the parser now wraps the aliased atom in a
  group carrying the outer quantifier and separator.

CSS::Writer's `t/01readme.t` now passes; `t/write-css.t` is down to one
assertion that needs Rakudo's `is_narrower` partial-order multi dispatch
(#11175).
