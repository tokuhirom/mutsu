use Test;

plan 4;

# `$@` is no longer a problem: it parses as an anonymous `$` and `@`, both
# benign, so it must not be flagged as an undeclared variable under EVAL.
lives-ok { EVAL '$@' }, '$@ is no longer a problem';

# Anonymous variables in general do not trip the EVAL undeclared check.
lives-ok { EVAL '$; @; %' }, 'bare anonymous sigils live';

# A genuinely undeclared variable is still reported.
throws-like '@a', X::Undeclared, symbol => '@a';
throws-like '%h', X::Undeclared, symbol => '%h';
