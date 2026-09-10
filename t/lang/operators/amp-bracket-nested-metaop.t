use Test;

plan 6;

# `&[op]` is the code-reference form of an infix operator. When the operator is
# a metaop whose operand is itself bracketed (`R[~~]` — the reverse of the reduce
# `[~~]`), the closing `]` must be matched with bracket balancing. mutsu found the
# FIRST `]`, so `&[R[~~]]` was cut to `R[~~` + a stray `]` and failed to parse.
# (Test::Run uses `&[R[~~]]`.)

my $rr = &[R[~~]];
ok $rr ~~ Callable, '&[R[~~]] parses to a callable (nested-bracket metaop)';

# The plain and single-metaop forms still work.
is (&[+])(2, 3), 5, '&[+] still works';
is (&[*])(4, 5), 20, '&[*] still works';
ok &[~~] ~~ Callable, '&[~~] still a callable';
ok &[R~~] ~~ Callable, '&[R~~] (single metaop, no inner bracket) still works';

# An unknown bare word inside &[...] is still a compile error.
ok (try { EVAL 'my $x = &[doesntexist]' } === Nil) && $! ~~ Exception,
    '&[doesntexist] still errors ("Missing infix inside []")';
