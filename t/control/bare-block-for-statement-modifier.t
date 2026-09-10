use v6;
use Test;

plan 24;

# A bare `{ ... }` block used as the operand of a statement modifier is the
# statement the modifier modifies -- it is run, not collected as an uncalled
# closure.  `{ BLOCK } for LIST` is exactly `for LIST { BLOCK }`.

# --- `for` modifier, expression position -------------------------------------

{
    my @r = ({ $_ + 1 } for 1, 2, 3);
    is-deeply @r, [2, 3, 4], 'bare block is invoked per element (expression position)';
}

{
    my $r = ({ $_ + 1 } for 1, 2, 3);
    isa-ok $r, List, 'the `for` modifier still yields a List';
    is $r.elems, 3, 'one result value per element';
    is $r[1], 3, 'result values are the block results, not Blocks';
}

# --- `for` modifier, statement position --------------------------------------

{
    my $n = 0;
    { $n++ } for 1 .. 3;
    is $n, 3, 'bare block runs once per element (statement position)';
}

# --- placeholder parameters --------------------------------------------------

{
    my @r = ({ $^a * 2 } for 1, 2, 3);
    is-deeply @r, [2, 4, 6], 'a single placeholder binds the element';
}

{
    my @r;
    { @r.push($^a ~ '/' ~ $^b) } for (1, 2), (3, 4);
    is-deeply @r, ['1 2/3 4'], 'two placeholders make the loop consume two elements';
}

{
    my @r = ({ $^a ~ '/' ~ $^b } for 1, 2, 3, 4);
    is-deeply @r, ['1/2', '3/4'], 'arity-2 placeholder block in expression position';
}

# --- the block/hash-literal disambiguation is untouched ----------------------

{
    my @r = ({ a => 1 } for 1, 2);
    is @r.elems, 2, 'a hash literal stays a hash literal';
    isa-ok @r[0], Hash, 'and is not invoked as a block';
    is @r[0]<a>, 1, 'the hash literal keeps its contents';
}

{
    my @r = ({ } for 1, 2);
    isa-ok @r[0], Hash, 'an empty `{ }` is an empty hash literal, not a block';
}

{
    # The array composer accepts the same inline modifier.
    my @r = [{ $_ * 2 } for 1, 2, 3];
    is-deeply @r, [2, 4, 6], 'bare block inside an array composer with `for`';
}

# --- other statement modifiers ----------------------------------------------

is ({ 42 } if 1), 42, 'bare block with the `if` modifier is invoked';
is ({ 42 } unless 0), 42, 'bare block with the `unless` modifier is invoked';
is ({ $_ * 2 } given 21), 42, 'bare block with the `given` modifier sees the topic';
is ({ $_ + 1 } with 41), 42, 'bare block with the `with` modifier sees the topic';

{
    my $n = 0;
    { $n = 5 } without Nil;
    is $n, 5, 'bare block with the `without` modifier is invoked';
}

{
    my $n = 0;
    { $n = 7 } if 0;
    is $n, 0, 'a false `if` modifier does not run the block';
}

# --- pointy / `sub` blocks are unaffected ------------------------------------

{
    my @r = (-> $x { $x * 2 } for 1, 2, 3);
    is-deeply @r, [2, 4, 6], 'a pointy block is still used as the loop body';
}

{
    my @r = (sub ($x) { $x * 2 } for 1, 2, 3);
    is-deeply @r, [2, 4, 6], 'a `sub` block is still used as the loop body';
}

{
    # A pointy block is NOT a bare block: the `if` modifier leaves it a term.
    my $r = (-> $x { $x } if 1);
    ok $r ~~ Callable, 'a pointy block with `if` stays an uncalled closure';
}

# `while`/`until` thunk the statement they modify, so a bare block stays an
# uncalled closure term there.
{
    my $i = 0;
    my @r = ({ 1 } while $i++ < 2);
    is @r.elems, 2, '`while` modifier still iterates';
    ok @r.all ~~ Callable, 'a bare block with `while` stays an uncalled closure';
}
