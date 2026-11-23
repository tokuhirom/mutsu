# RakuAST regex origins: counts, colon backtracking, leaders, backreferences

The origin walk of a regex body (`.origin` of `RakuAST::Regex::*` nodes) used
to stop at the first spelling it did not read. That left every later node
without a range. It now reads:

- an internal modifier with a count (`:i(0)`, `:1i`; a zero count negates);
- a colon backtracking modifier after a quantifier (`*:!`, `*:?`), including
  before a range's bounds (`**:!2..4`);
- a leading `|`, `||`, `&` or `&&` in a regex body or group, and the `&` that
  may open an alternation branch (`a |& b`);
- a positional backreference (`$0`);
- counts in any script's decimal digits (`$٠`, `a**۳`).

The ranges match rakudo 2026.11's. (#18739)
