# A plain single-quoted `-->` literal return spec is built directly

A routine declared `--> ''` or `--> 'text'` returns that string. `evaluate_definite_return_value`
built the spec's constants directly for `Nil`, `True`, `False`, `Empty`, `pi` and integers, but a
string literal went through `eval_eval_string`: a fresh compilation unit, a parse and an isolated
run, on every return. `RakuAST::Deparse` declares four empty-body candidates `--> ''` and they are
reached for every `:U` / empty node, so over three rounds of six `DEPARSE` snippets the string
EVAL was 23% of the run (1.14G instructions, mostly `parse_program`).

A single-quoted literal with no backslash and no embedded quote now yields `Value::str` of its text.
Anything with a backslash (`'a\0b'`, `'it\'s'`) or a double-quoted literal (which may interpolate)
keeps the EVAL path, so the value and its type are unchanged: the new test compares both against
`raku` and the previous binary.

Measured on the debug binary: `t/rakuast/rakuast-deparse.t` 42 s -> 31 s.
