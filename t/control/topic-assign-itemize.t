use Test;

# Assigning an aggregate to the topic `$_` itemizes it when the topic aliases
# a Scalar (an element, a `$` variable, the topic's own container), but not
# when it aliases an `@`/`%` container itself (#11229).

plan 8;

{
    my @a = 1;
    for @a { $_ = [1, 2] }
    is @a[0].raku, '$[1, 2]', 'for: an Array assigned to the topic is itemized in the element';
}

{
    my @a = 1;
    for @a { $_ = %(a => 1) }
    is @a[0].raku, '${:a(1)}', 'for: a Hash assigned to the topic is itemized in the element';
}

{
    my $t = 1;
    given $t { $_ = [1] }
    is $t.raku, '$[1]', 'given $var: the aliased Scalar holds an itemized Array';
}

{
    my @a = 1, 2;
    @a.map({ $_ = [7] });
    is @a.raku, '[[7], [7]]', 'map: each element is replaced by an itemized Array';
    is @a.elems, 2, 'map: the itemized elements do not flatten';
}

{
    $_ = [1, 2];
    is $_.raku, '$[1, 2]', 'a plain topic assignment itemizes';
}

{
    my @c = 1, 2;
    given @c { .=reverse }
    is @c.raku, '[2, 1]', 'given @a: a write-back through the topic stays un-itemized';
}

{
    my %h = a => 1;
    for %h.values { $_ = [1] }
    is %h.raku, '{:a($[1])}', 'for %h.values: the hash value is itemized';
}
