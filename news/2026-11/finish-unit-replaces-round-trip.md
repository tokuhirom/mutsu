# The frontend entry is `finish_unit`, not a "round trip"

The executing path stopped building a `RakuAstNode` tree for a whole unit long
ago (#15744): the parser keeps the migrated families as typed nodes and
`rakuast::finalize` lowers each where it stands. The entry points still carried
the old name, `frontend::round_trip_unit` and its `round_trip`,
`round_trip_owned` and `round_trip_eval` forms, which suggested a
convert-everything-and-read-it-back pipeline that no longer exists.

ADR-15760 §2.5 deletes those names. They are now `frontend::finish_unit` (a
parsed unit, default scope), `finish_unit_in` (a unit in a given `NameScope`)
and `finish_eval_unit` (an `EVAL` string's unit, finalized in the scope its
caller makes); the borrowed `round_trip` form had only test callers and is gone.
No behaviour changes (#15760).
