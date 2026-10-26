use v6;
use Test;

plan 29;

# A bare `{ ... }` block used as the operand of a statement modifier is the
# statement the modifier modifies -- it is run, not collected as an uncalled
# closure.  `{ BLOCK } for LIST` is exactly `for LIST { BLOCK }`.  Under `if` /
# `unless` / `given` / `with` / `without` a block in expression position is a
# plain term (rakudo 2026.09), and a pointy block is called with the tested value.

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

ok ({ 42 } if 1) ~~ Block, 'bare block with the `if` modifier stays a Block in a term';
ok ({ 42 } unless 0) ~~ Block, 'bare block with the `unless` modifier stays a Block';
ok ({ $_ * 2 } given 21) ~~ Block, 'bare block with the `given` modifier stays a Block';
ok ({ $_ + 1 } with 41) ~~ Block, 'bare block with the `with` modifier stays a Block';

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
    # A routine is a value, not a block: the loop builds it once per element
    # and calls nothing (rakudo 2026.09).
    my @r = (sub ($x) { $x * 2 } for 1, 2, 3);
    ok @r.elems == 3 && @r.all ~~ Sub, 'a `sub` is a value the loop does not call';
}

{
    # A pointy block is called with the value `if` / `unless` / `with` tested.
    is (-> $x { $x } if 1), 1, 'a pointy block with `if` is called with the condition';
    is (-> $x { $x } unless 0), 0, 'a pointy block with `unless` is called with the condition';
    is (-> $x { $x } with 42), 42, 'a pointy block with `with` is called with the value';
    ok (-> $x { $x } given 5) ~~ Callable, 'a pointy block with `given` stays a closure';
    is (-> $x { $x } if 0).elems, 0, 'a false `if` does not call it';
}

# `while`/`until` run a bare block on each pass and collect its value.
{
    my $i = 0;
    is-deeply ({ 1 } while $i++ < 2).List, (1, 1), 'a bare block with `while` is run';
    $i = 0;
    is-deeply ({ $i } until $i++ >= 2).List, (1, 2), 'a bare block with `until` is run';
    $i = 0;
    my $n = 0;
    { $n += 2 } while $i++ < 2;
    is $n, 4, 'at statement level the `while` block runs too';
}
