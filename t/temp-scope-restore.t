use Test;

plan 11;

# `temp` saves a variable's value and restores it when the enclosing scope
# exits. These cover the cases that previously failed: restoration on *sub*
# exit, recursive temps, `++temp`, the `TEMP { }` phaser, multi-level indexed
# lvalues, and temping a non-existent dynamic.

# Restoration on bare-block exit.
{
    my $a = 42;
    { temp $a = 23; is $a, 23, 'temp changed the variable in a block'; }
    is $a, 42, 'temp restored the variable after the block';
}

# Restoration on *sub* exit (no explicit return).
{
    my $value = 0;
    my sub non-recursive { temp $value = $value + 1; }
    non-recursive();
    is $value, 0, 'temp restored on sub exit';
}

# Restoration across recursion.
{
    my $value = 0;
    my sub recursive(Int $limit) {
        temp $value = $value + 1;
        recursive($limit - 1) if $limit > 0;
    }
    recursive(10);
    is $value, 0, 'temp restored across recursive calls';
}

# `++temp $x`: temporize and increment in one expression.
{
    my $depth = 0;
    my $c = 1;
    sub a {
        ++temp $c;
        a() if ++$depth < 3;
    }
    a();
    is $c, 1, 'recursive nested ++temp restored properly';
}

# `TEMP { }` phaser is NYI (matches Rakudo): the block never runs.
{
    my $next = 0;
    sub advance() {
        my $curr = $next++;
        TEMP {{ $next = $curr }}
        return $curr;
    }
    is advance(), 0, 'TEMP block does not restore (1)';
    is advance(), 1, 'TEMP block does not restore (2)';
    is $next, 2, 'TEMP block leaves $next advanced';
}

# Multi-level indexed lvalue.
{
    my $struct = [ "x", { key => [ "y", 42 ] } ];
    { temp $struct[1]<key>[1] = 23;
      is $struct[1]<key>[1], 23, 'temp changed a nested array/hash element'; }
    is $struct[1]<key>[1], 42, 'temp restored the nested element';
}

# Temping a never-declared dynamic variable throws.
throws-like { temp $*undeclared-dyn = 42 }, X::Dynamic::NotFound,
    'temp on a non-existent dynamic throws X::Dynamic::NotFound';

# vim: expandtab shiftwidth=4
