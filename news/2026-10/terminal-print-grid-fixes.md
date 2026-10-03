# Terminal::Print: shared row splices, nested `.join`, dependent named defaults

Three fixes, found through Terminal::Print's `t/20-boxdrawing.t`, which now
passes 35/35:

- **A scalar holding an element's array shares it for `pop`, `shift` and
  `splice` too.** `my $row = @grid[0]; $row.splice(0, 2, @cells)` rebuilt a
  detached copy under `$row`'s name, so the grid row never changed; `push`
  already mutated the shared array in place. The three mutators now do the
  same, through a new `Value::with_array_data_in_place` helper.
- **`.join` runs a user `.Str` inside a nested list.** `([$cell],).join`
  stringified the inner array with the built-in renderer, printing the
  instance's default `Cell<id>` form instead of its `method Str`. Both the
  native fast path and the interpreter's `join` now route an element through
  `.Str` when the element (at any depth) holds an instance
  (`value::gist::str_needs_dispatch`).
- **A named parameter's default may read an earlier named parameter during
  dispatch.** Matching an absent `:$corners where CORNERS|Positional =
  WEIGHT{$style}` evaluated the default before `$style` was bound, so it saw
  `Any`, warned, failed the `where`, and the multi call died with "Cannot
  resolve caller". The matcher now binds earlier named parameters (to their
  arguments, or their own defaults) before evaluating such a default.
