use Test;

# `given %h<k> { ... }` / `given @a[i] { ... }` topicalize a container *element*
# by alias: the element is an lvalue, so BOTH a whole reassign (`$_ = ...`) and
# a container mutation (`.push`) through `$_` propagate back to that element.
# (This differs from a whole-container topic `given @a`, which is read-only for
# reassign.) The final `$_` is written back to the element after the body.

plan 16;

# --- hash element: reassign and mutate -----------------------------------
{
    my %h = k => 10;
    given %h<k> { $_ = 99 }
    is %h<k>, 99, 'given %h<k> { $_ = v } assigns the element';
}
{
    my %h = k => [1, 2];
    given %h<k> { .push(3) }
    is %h<k>.gist, '[1 2 3]', 'given %h<k> { .push } propagates to the element';
}

# --- array element: reassign and mutate ----------------------------------
{
    my @a = 1, 2, 3;
    given @a[1] { $_ = 50 }
    is @a.gist, '[1 50 3]', 'given @a[i] { $_ = v } assigns the element';
}
{
    my @a = [1], [2], [3];
    given @a[1] { .push(22) }
    is @a[1].gist, '[2 22]', 'given @a[i] { .push } propagates to the element';
}

# --- runtime (variable) subscripts ---------------------------------------
{
    my %h = foo => [1, 2];
    my $k = 'foo';
    given %h{$k} { .push(3) }
    is %h<foo>.gist, '[1 2 3]', 'given %h{$k} (variable key) propagates';
}
{
    my @a = [1], [2], [3];
    my $i = 2;
    given @a[$i] { .push(9) }
    is @a[2].gist, '[3 9]', 'given @a[$i] (variable index) propagates';
}

# --- the element topic is rw: $_ assign is allowed (not read-only) --------
{
    my @a = 1, 2, 3;
    lives-ok { given @a[0] { $_ = 100 } },
        'given @a[i] { $_ = v } is rw (element is an lvalue)';
}

# --- read-only usage leaves the element unchanged ------------------------
{
    my %h = x => 5;
    my $sum = 0;
    given %h<x> { $sum += $_ }
    is %h<x>, 5, 'read-only given element leaves the element unchanged';
    is $sum, 5, 'read-only given element still reads the topic';
}

# --- nested given over distinct elements ---------------------------------
{
    my %m = a => [1], b => [2];
    given %m<a> {
        given %m<b> { .push(20) }
        .push(10);
    }
    is %m<a>.gist, '[1 10]', 'nested given element: outer propagates';
    is %m<b>.gist, '[2 20]', 'nested given element: inner propagates';
}

# --- scalar holding a container, element topic ---------------------------
{
    my $h = { y => [7] };
    given $h<y> { .push(8) }
    is $h<y>.gist, '[7 8]', 'given $h<k> (scalar holding hash) propagates';
}

# --- given element with when/default dispatch still works -----------------
{
    my %w = k => 5;
    my $res = do given %w<k> {
        when * > 3 { "big" }
        default { "small" }
    };
    is $res, 'big', 'given %h<k> { when ... } dispatch works';
}

# --- the element value is the topic (single item, not flattened) ----------
{
    my @a = <a b c>;
    my @nested = @a, <d e>;
    my @seen;
    given @nested[0] { @seen.push(.elems) }
    is @seen.gist, '[3]', 'given @a[i] topicalizes the element as one item';
}

# --- mutating the element does not disturb siblings -----------------------
{
    my @a = [1, 2], [3, 4];
    given @a[0] { .push(99) }
    is @a[0].gist, '[1 2 99]', 'sibling element untouched: target updated';
    is @a[1].gist, '[3 4]', 'sibling element untouched: other unchanged';
}
