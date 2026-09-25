use Test;

# `⚛` (atomic fetch) composes with any symbolic prefix operator in front of
# it, the way rakudo parses it (#9324, FFmpegProgressBar's
# `return +⚛$!force-exit-code;`).

plan 8;

my atomicint $x = 3;
is ⚛$x, 3, 'bare atomic fetch';
is +⚛$x, 3, '+⚛$x';
is -⚛$x, -3, '-⚛$x';
is ~⚛$x, '3', '~⚛$x';
is ?⚛$x, True, '?⚛$x';
is !⚛$x, False, '!⚛$x';

class C {
    has atomicint $!code = 7;
    method code() { return +⚛$!code }
    method neg()  { -⚛$!code }
}
is C.new.code, 7, '+⚛$!attr';
is C.new.neg, -7, '-⚛$!attr';
