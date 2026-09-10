use Test;

plan 7;

# A `.=` metaop on the topic must respect read-only topics. When the topic
# aliases an immutable value (a Range element from `for ^N`, or a bare scalar
# value in `given`), `.=` must die rather than silently mutate. When it aliases
# a mutable `@`/`%` container (`given @a`/`with %h`) or a mutable array element
# (`for @a`), `.=` must write through.

# for over a Range: topic is read-only
{
    my $c = 0;
    dies-ok { for ^8 { .=fmt('%03b'); $c++ } }, 'for ^N: .= on read-only topic dies';
    is $c, 0, '... and the loop did not advance past the first immutable element';
}

# given a bare immutable value: topic is read-only
dies-ok { given 42 { .=succ } }, 'given VALUE: .= on read-only topic dies';

# given @a: topic aliases the whole mutable container, so .= writes through
{
    my @a = 1, 2, 3;
    given @a { .=reverse }
    is-deeply @a, [3, 2, 1], 'given @a: .= writes through to the container';
}

# with %h: .= on the whole-hash topic writes through
{
    my %h = a => 1, b => 2;
    with %h { .=Hash }
    is %h<a>, 1, 'with %h: .= keeps the container usable';
}

# for @a: topic aliases a mutable array element, so .= mutates it
{
    my @a = 1, 2, 3;
    for @a { .=succ }
    is-deeply @a, [2, 3, 4], 'for @a: .= mutates each array element';
}

# for @a: plain assignment to the topic also writes through (sanity)
{
    my @a = 1, 2, 3;
    for @a { $_ = 99 }
    is-deeply @a, [99, 99, 99], 'for @a: $_ = ... writes through';
}
