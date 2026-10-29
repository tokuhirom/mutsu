# `COND ?? ($s = V if $c) !! $x ~= W` no longer dies

A compound assignment whose target is a ternary pushes the operator into the
selected branch. A branch written `($s = V if $c)` is a statement-modifier
conditional with an lvalue body; the RakuAST expansion handles it, but the
parser's ternary recursion did not reach it, and the parser kept the body as a
statement-level assignment the expansion did not accept. Both now go through the
shared expansion, so an unrun modifier is a silent no-op as in rakudo. This
takes P5reset's `t/01-basic.rakutest` from 4 to 7 of 7.
