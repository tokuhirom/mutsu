use Test;

# The loose word-logicals (`and`/`or`/`xor`/`andthen`/`orelse`/`notandthen`) are
# the LOOSEST infix operators — looser than item assignment (`=`). This is
# already covered for a plain `$x`/`@a`/`%h` target in
# t/word-logical-loosest-precedence.t. This file covers every *other* lvalue
# shape (an attribute accessor, an indexed element, a hash key), which used to
# swallow the operator into the RHS and store the wrong value:
#
#   $o.v = 1 andthen 0   stored 0 (the tail) instead of 1.
#
# The assignment binds first and its (assigned) value drives the short-circuit:
# `($o.v = 1) andthen 0` runs `andthen 0` with the store already done, so the
# attribute keeps `1` while the statement value is `0`.

plan 19;

class C { has $.v is rw }

# --- method / attribute lvalue ---
{
    my $o = C.new;
    $o.v = 1 andthen 0;
    is $o.v, 1, '$o.v = 1 andthen 0 -> ($o.v = 1) andthen 0, attribute stays 1';
}
{
    my $o = C.new;
    $o.v = 5 or die "boom";
    is $o.v, 5, '$o.v = 5 or die -> die never runs, attribute is 5';
}
{
    my $o = C.new;
    $o.v = 1 notandthen 99;
    is $o.v, 1, '$o.v = 1 notandthen 99 -> attribute is 1';
}

# --- positional (array element) lvalue ---
{
    my @a;
    @a[0] = 8 andthen 0;
    is @a[0], 8, '@a[0] = 8 andthen 0 -> element stays 8';
}
{
    my @a;
    @a[0] = 5 orelse 9;
    is @a[0], 5, '@a[0] = 5 orelse 9 -> element stays 5';
}
{
    my @a;
    @a[0] = 3 or die "boom";
    is @a[0], 3, '@a[0] = 3 or die -> die never runs';
}

# --- associative (hash key) lvalue ---
{
    my %h;
    %h<k> = 9 andthen 0;
    is %h<k>, 9, '%h<k> = 9 andthen 0 -> key stays 9';
}
{
    my %h;
    %h{'k'} = 7 andthen 0;
    is %h{'k'}, 7, '%h{k} = 7 andthen 0 -> key stays 7';
}

# --- the statement *value* is still the tail (word-logical wraps the whole assignment) ---
{
    my $o = C.new;
    my $r = ($o.v = 5 andthen 0);
    is $r, 0, 'the parenthesized expression value is the andthen tail (0)';
    is $o.v, 5, '...but the attribute store still happened (5)';
}
{
    my @a;
    my $r = (@a[0] = 8 andthen 42);
    is $r, 42, 'expression value is the andthen tail (42)';
    is @a[0], 8, '...but the element store still happened (8)';
}

# --- these were always correct (short-circuit accident); keep them green ---
{
    my $o = C.new;
    $o.v = 0 orelse 9;
    is $o.v, 0, '$o.v = 0 orelse 9 -> 0 is defined, store is 0';
}

# --- other RHS forms must keep working under the new no-word-logical RHS parse ---
{
    my @a;
    @a[0] = 1 ?? 8 !! 9;
    is @a[0], 8, 'ternary RHS still binds inside the assignment';
}
{
    my @a;
    @a = 1, 2, 3 and 99;
    is-deeply @a, [1, 2, 3], '@a = 1,2,3 and 99 -> (@a = 1,2,3) and 99';
}
{
    my @a;
    @a[0,1,2] = 1, 2 ... 10;
    is-deeply @a, [1, 2, 3], 'sequence RHS still folds the whole comma list';
}
{
    my @a = 10, 20;
    @a[0] += 5 andthen 0;
    is @a[0], 15, 'compound-assign element: (@a[0] += 5) andthen 0';
}
{
    my @a;
    my $u;
    @a[0] = $u = 42;
    is @a[0], 42, 'chained assignment through an element still works (element)';
    is $u, 42, 'chained assignment through an element still works (scalar)';
}
