# `foo: args` at statement start is a label, and a labelled statement has a value

`sub w() { trim: "hi" }` called `trim` in mutsu when a sub of that name was
declared (a "colon-listop" reading), so `w()` returned `hi!`. Rakudo -- both
frontends -- reads a statement-initial `IDENT:` followed by whitespace as a
statement label whatever `IDENT` names: the statement is `"hi"` and `w()`
returns `hi`. The declared-sub special case in the label parser is gone, and
`t/lang/adverbs/colon-listop-call.t` now asserts rakudo's answers (#18283).

That exposed a second divergence: a labelled statement in value position
evaluated to `Nil` (`sub w() { FOO: 42 }` returned `Nil`, as did
`do { FOO: 42 }`, `(FOO: 42)` and `EVAL 'FOO: 42'`). The compiler's
tail-value paths now see through statement labels -- the label still marks
its `goto` target, and the value is the labelled statement's.
