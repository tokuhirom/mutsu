# A grammar's `FAILGOAL` is invoked again when a `~` goal goes missing

`grammar A { token TOP { '[' ~ ']' \w+ }; method FAILGOAL($goal) { die ... } }`
parsed `'[good]'` fine but produced nothing at all — no exception, no message —
for the failing `A.parse: '[bad'`, where raku reports
`X::AdHoc: Cannot find ']'  near position 4`.

## Root cause

The machinery was already there and was already firing: a `GoalMatch` atom whose
opener matched but whose closer did not calls `record_goal_failure`, and
`dispatch_package_parse` consulted that record on the main-match failure path and
dispatched `FAILGOAL`.

It never got there. For this grammar the parse dies *earlier*: LTM candidate
selection (`eval_token_call_values_at`) runs the start rule's pattern to rank it,
finds no declarative match, and returns `Ok(None)`. That branch went straight to
`parse_failure_for_pattern(&text, None)` and never looked at the pending goal
failure — so the very run that recorded the goal was the one that skipped
reporting it.

Three further gaps surfaced once the call was reached:

- **The goal text.** Rakudo hands `FAILGOAL` the goal's *source* text including
  the whitespace separating it from the conjunction's content, so `'[' ~ ']' \w+`
  reports `"']' "` — that trailing space is where the doubled space in
  `Cannot find ']'  near position 4` comes from. mutsu only had the matcher's
  reconstructed atom text (`']'`), which cannot carry the spacing. The goal is now
  refined against the rule's source, which also handles the `rule` case where
  sigspace has already rewritten the separator into an explicit `<.ws>` atom.
- **Errors thrown from inside `FAILGOAL` were swallowed.** The dispatch treated
  *any* `is_method_not_found()` error as "this grammar has no `FAILGOAL`". A
  `FAILGOAL` whose body raised a method-not-found error of its own (the ticket's
  did — see below) therefore looked like an absent method. The presence of the
  method is now checked up front, and every error from its body propagates.
- **`self.pos` inside `FAILGOAL`.** Rakudo calls `FAILGOAL` on the *cursor*, so
  `self.pos` is where the goal went missing. mutsu passed the grammar type object,
  and `self.pos` died. The invocant is now an instance of the grammar carrying the
  Cursor positional state (`pos`/`from`/`to`/`orig`/`target`), and the generic
  accessor path answers those names for any instance whose MRO includes `Grammar`
  — a user-declared class otherwise only answers its own declared attributes.

Verified against raku for the spaced and tight forms, `rule` and `token`, a
subrule goal (`<.close> `), nested goals (the innermost failing goal wins, with
its own position), and the no-`FAILGOAL` case (which stays a plain failed parse).

Pin: `t/grammar-dynvar-failgoal-ws.t`.
