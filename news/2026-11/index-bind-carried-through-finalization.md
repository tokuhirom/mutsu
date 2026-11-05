# Indexed binds are carried through finalization

`@a[0] := $v` and `%h<k> := $v` (an `IndexAssign` holding an `IndexBindValue`) are kept as the
parser built them by `finalize_unit` instead of being converted to a RakuAST `:=` infix and
lowered back to the same tree (ADR-15760 slice 2). The bound value's marker is the compiler's own
node, so the walk now reaches the right-hand side where it stands. The `finalization_is_the_ordinary_tree`
test pins that a unit with indexed binds finalizes to the ordinary parse; `t/rakuast/rakuast-index-bind-finalize.t`
pins the semantics.
