# A `nqp::bindattr`-bound attribute survives a same-named `my` in a call's arguments

On method exit the `:=`-binding recovery adopted a bare-named cell as the binding of the
attribute of that name unless the frame declared a `my` of it. A `my` written inside an
expression (`nqp::push($!stats, my $stats := $x)`) is recorded apart from statement-level
ones, so the check missed it and the attribute `$!stats` was replaced by the value of the
lexical `$stats` (ParaSeq's `!update-stats`, #16866).
