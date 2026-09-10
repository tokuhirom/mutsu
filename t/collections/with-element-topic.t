use Test;

# `with %h<k> { ... }` / `with @a[i] { ... }` topicalize a container *element*
# by alias (same as `given` on an element): the element is an lvalue, so BOTH a
# whole reassign (`$_ = ...`) and a container mutation (`.push`) through `$_`
# propagate back to that element. `with` only runs the body when the element is
# defined.

plan 12;

# --- hash element: mutate and reassign -----------------------------------
{
    my %h = k => [1, 2];
    with %h<k> { .push(3) }
    is %h<k>.gist, '[1 2 3]', 'with %h<k> { .push } propagates to the element';
}
{
    my %h = k => 10;
    with %h<k> { $_ = 99 }
    is %h<k>, 99, 'with %h<k> { $_ = v } assigns the element';
}

# --- array element: mutate and reassign ----------------------------------
{
    my @a = [1], [2];
    with @a[0] { .push(9) }
    is @a[0].gist, '[1 9]', 'with @a[i] { .push } propagates to the element';
}
{
    my @a = 1, 2, 3;
    with @a[1] { $_ = 50 }
    is @a.gist, '[1 50 3]', 'with @a[i] { $_ = v } assigns the element';
}

# --- runtime (variable) subscripts ---------------------------------------
{
    my %h = foo => [1];
    my $k = 'foo';
    with %h{$k} { .push(2) }
    is %h<foo>.gist, '[1 2]', 'with %h{$k} (variable key) propagates';
}
{
    my @a = [1], [2];
    my $i = 1;
    with @a[$i] { .push(8) }
    is @a[1].gist, '[2 8]', 'with @a[$i] (variable index) propagates';
}

# --- with only runs when the element is defined --------------------------
{
    my %u;
    my $ran = 0;
    with %u<missing> { $ran = 1 }
    is $ran, 0, 'with undefined element skips the body';
}
{
    my %d = k => 5;
    my $ran = 0;
    without %d<k> { $ran = 1 }
    is $ran, 0, 'without defined element does not run';
}

# --- the element topic is rw (not read-only) -----------------------------
{
    my @a = 1, 2, 3;
    lives-ok { with @a[0] { $_ = 100 } },
        'with @a[i] { $_ = v } is rw (element is an lvalue)';
}

# --- read-only usage leaves the element unchanged ------------------------
{
    my %h = x => 5;
    my $sum = 0;
    with %h<x> { $sum += $_ }
    is %h<x>, 5, 'read-only with element leaves the element unchanged';
    is $sum, 5, 'read-only with element still reads the topic';
}

# --- scalar holding a container, element topic ---------------------------
{
    my $h = { y => [7] };
    with $h<y> { .push(8) }
    is $h<y>.gist, '[7 8]', 'with $h<k> (scalar holding hash) propagates';
}
