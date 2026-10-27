use lib 't/lib';
use Test;

# The bare `X` / `Z` metaoperators are the routines `infix:<X>` / `infix:<Z>`:
# a `multi` candidate a `use` imports joins the core candidate and takes the
# call it accepts, in every statement shape. (Found with Red's
# `Model.^all X Other.^all` in a `my $x = (...).grep({ ... })`, which ran the
# core cross product because the RakuAST lowering cannot see the import.)

use MetaopUserInfix;

plan 14;

my $t1 = table('a');
my $t2 = table('b');

is ($t1 X $t2), 'cross of 2: a+b', 'X over imported-type operands calls the candidate';
my $d = ($t1 X $t2).grep({ True });
is $d, 'cross of 2: a+b', 'in a declaration initializer with a block argument';
my $e = ($t1 X $t2).chars;
is $e, 15, 'in a declaration initializer with a method call';
is ($t1 Z $t2), 'zip of 2: a+b', 'Z calls its candidate';
is ($t1 X $t2 X $t1), 'cross of 3: a+b+a', 'a chain is one call of the list-associative routine';
is ($t1 Z $t2 Z $t1), 'zip of 3: a+b+a', 'a Z chain is one call';

# No candidate accepts these operands: the core operators run.
is-deeply ((1, 2) X (3, 4)).list, ((1, 3), (1, 4), (2, 3), (2, 4)).list, 'core X when no candidate accepts';
is-deeply ((1, 2) X (3, 4) X (5,)).list.elems, 4, 'core X chain';
is-deeply ((1, 2) Z (3, 4)).list, ((1, 3), (2, 4)).list, 'core Z when no candidate accepts';
my @cross = (1, 2) X (3, 4);
is @cross.elems, 4, 'core X in an array initializer';
is-deeply ((1, 2) X+ (10, 20)).list, (11, 21, 12, 22).list, 'X+ is the core meta operator';
is-deeply ((1, 2) Z~ <a b>).list, ('1a', '2b').list, 'Z~ is the core meta operator';

{
    my $r = ($t1 X $t2).grep({ True });
    is $r, 'cross of 2: a+b', 'inside a nested block';
}
is (1, 2, 3).map({ ($t1 X $t2).chars }).sum, 45, 'in a closure body';

done-testing;
