use Test;

# From Test::Stream::Predicator.cmp-ok: EVAL of `&infix:<word>` for an
# undeclared word operator throws instead of yielding a stub routine.

plan 4;

throws-like { EVAL '&infix:<not-an-op>' }, X::Undeclared::Symbols, 'undeclared word infix';
is (EVAL '&infix:<eq>')('a', 'a'), True, 'a builtin word infix resolves';
is (EVAL '&infix:<+>')(1, 2), 3, 'a symbolic infix resolves';
sub infix:<zork>($a, $b) { "z$a$b" }
is (EVAL '&infix:<zork>')(1, 2), 'z12', 'a user-declared word infix resolves';
