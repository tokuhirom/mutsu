use Test;

# "Two terms in a row" was only diagnosed when the WHOLE STATEMENT was a bare
# value expression (`1 1;`). A `my` initializer (`my $x = 1 1;`) and a listop
# argument (`say 1 1;`) never reached the check: the initializer/argument
# parser stopped at the first complete term and the leftover term was
# silently re-read as a new statement, which then just warned about sink
# context instead of raising a compile error.
# See todo/tickets/two-terms-in-a-row-is-not-a-parse-error.md.

plan 15;

# --- Now-fixed error cases -------------------------------------------------

throws-like 'my $x = 1 1;', X::Syntax::Confused, 'my initializer: two terms in a row (scalar)',
    reason => 'Two terms in a row';
throws-like 'my @a = 1 1;', X::Syntax::Confused, 'my initializer: two terms in a row (array)',
    reason => 'Two terms in a row';
throws-like 'my $x = 1, 2 3;', X::Syntax::Confused,
    'my initializer: two terms in a row after trailing comma sink',
    reason => 'Two terms in a row';
throws-like 'say 1 1;', X::Syntax::Confused, 'say: two terms in a row',
    reason => 'Two terms in a row';
throws-like 'say "a" "b";', X::Syntax::Confused, 'say: two terms in a row (strings)',
    reason => 'Two terms in a row';
throws-like 'sub f(*@a) { }; f 1 1;', X::Syntax::Confused,
    'user-sub listop call: two terms in a row',
    reason => 'Two terms in a row';

# --- Regression guards: legitimate continuations must keep working --------

is-deeply +(EVAL 'my $x = 1 but True; $x'), 1, 'my $x = 1 but True still parses';
is-deeply (EVAL 'my $x = 1 if True; $x'), 1, 'my $x = 1 if True still parses';
is-deeply (EVAL 'my $x = 1 for ^1; $x'), 1, 'my $x = 1 for LIST still parses';
is-deeply (EVAL 'my $x = 1, 2, 3; $x'), 1, 'my $x = 1, 2, 3 (trailing sink) still parses';

lives-ok { EVAL 'say 1, 2;' }, 'say with comma-separated args still parses';
lives-ok { EVAL 'say 1, :foo;' }, 'say with a trailing adverb still parses';
lives-ok { EVAL 'say 1 for ^3;' }, 'say with a following statement modifier still parses';
lives-ok { EVAL 'sub f($x, :$foo) { }; f 1, :foo(2);' },
    'user-sub listop call with comma + adverb still parses';
lives-ok { EVAL 'sub f(*@a) { }; f 1, 2;' },
    'user-sub listop call with comma-separated args still parses';
