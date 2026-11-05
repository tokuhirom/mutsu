# The user lexical `$self` and Pod variables are carried through finalization

ADR-15760 slice 2: the user lexical `$self` (stored under the sigiled env key, ADR-0061) and
the Pod variables (`$=pod`, `$=finish`) read back as the same name after the RakuAST round
trip, so `finalize_unit` keeps them as the parser built them instead of converting each to a
variable node and lowering it back. A census of 466 fallback expressions put them at 24, after
the regex literals (385, tracked by #8033) and ahead of the index assignments.
