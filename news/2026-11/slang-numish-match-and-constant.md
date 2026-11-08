# A slang's `numish` override sees the literal's text and capture keys; `RakuAST::Constant`

The `$/` a slang's `numish` action override receives (ADR-15762) is now the literal's text with the
capture the core grammar has (`integer` or `decimal-number`, `radix-number` for `:16<ff>`), so an
override can read it by key as `overload::constant` does (`nqp::atkey(nqp::findmethod($/, 'hash')($/),
'integer')`). `RakuAST::Constant.new($value)` exists (a literal node that holds an arbitrary
compile-time value; `.value`, `EVAL`), which is how that module hands a handler's result back. A
faithful fixture of the module's mechanisms now matches `raku` (#15804); the literal's original
spelling for `1e3` / `0x2A` is #17114.
