use Test;

# A destructive `s///` mutates its topic in place. When the topic is bound
# read-only (a scalar `$_` / named parameter without `is rw`/`is copy`), an
# actual substitution must throw X::Assignment::RO. A non-matching `s///`
# writes nothing and stays a no-op.

plan 5;

# matching s/// on a read-only $_ parameter dies
{
    sub ro-topic($_) { s/c// }
    dies-ok { ro-topic('ccc') }, 's/// on a read-only $_ parameter dies';
}

# non-matching s/// on a read-only $_ parameter is a no-op (lives)
{
    sub ro-topic-nomatch($_) { s/z// }
    lives-ok { ro-topic-nomatch('ccc') },
        'non-matching s/// on a read-only topic is a no-op';
}

# the thrown exception is X::Assignment::RO
{
    sub ro-topic2($_) { s/c/X/ }
    my $ex = (try { ro-topic2('ccc') }) // $!;
    isa-ok $ex, X::Assignment::RO, 's/// RO violation is X::Assignment::RO';
}

# a mutable topic still substitutes normally
{
    $_ = 'ccc';
    s/c/X/;
    is $_, 'Xcc', 's/// on a mutable topic mutates it';
}

# :g over a mutable loop topic still works
{
    my @a = <ccc ddd>;
    for @a { s:g/c/X/ }
    is @a.join(','), 'XXX,ddd', ':g s/// over a mutable loop topic';
}
