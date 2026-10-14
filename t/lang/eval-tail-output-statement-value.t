use v6;
use Test;

plan 6;

# The value of an EVAL unit whose last statement is a listop say/put/print/note
# is True, like the call form (#13319).
my $out = "";
{
    my $*OUT = class { method print(*@a) { $out ~= @a.join; True } method flush { } }.new;
    is-deeply EVAL(q[say 1]), True, 'say listop tail is True';
    is-deeply EVAL(q[put 2]), True, 'put listop tail is True';
    is-deeply EVAL(q[print 3]), True, 'print listop tail is True';
}
is-deeply EVAL(q[note "n" if False; 5]), 5, 'a later value statement still wins';
is-deeply EVAL(q[(say 1)]), True, 'parenthesised form';
is-deeply Q[say 3].AST.EVAL, True, 'AST.EVAL of a say statement';
