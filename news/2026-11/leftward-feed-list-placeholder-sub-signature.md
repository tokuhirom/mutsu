# Leftward feeds take the whole list; placeholder subs have no signature

A feed is looser than the comma, but a leftward feed in an expression took
only the first item on its right: `@p <== grep { $_ } <== 3, 4` left `@p`
as `[3]` and warned about a useless `4`. The source of `<==` / `<<==` is now
the whole comma list (`parser::expr::precedence::feed::leftward_source_list`),
as a declaration feed's already was.

`.origin` (#15780): the declaration a leftward feed starts with
(`my @o <== map { $_ } <== 1, 2`) runs up to its first arrow, and the
`Feed` is placed (`rakuast::feed_origins`).

A `sub` with no written signature whose body declares placeholders
(`sub { $^a ~ "x" }`) has no `Signature` until it is compiled: Rakudo's
`.signature` is the type object and `visit-children` skips it. mutsu filled
in an empty one, as it does for `sub { 1 }`; it no longer does
(`empty_fields::stays_undefined`).
