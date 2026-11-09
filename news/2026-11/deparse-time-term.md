# `time` deparses as a named term

`time` is `RakuAST::Term::Named` (like `now`), so `.DEPARSE` prints `time`, not `time()`,
and `Term::Named.new("time")` lowers back to the call.
