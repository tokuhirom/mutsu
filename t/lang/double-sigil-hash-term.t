use Test;

plan 9;

# `%%` in term position is the anonymous hash (an empty Hash), the hash
# counterpart of `@@`; `%%name` is `%name` in hash context. In infix
# position `%%` stays the divisibility operator. (#9319)

{
    my %h = %%;
    is %h.elems, 0, 'my %h = %% gives an empty hash';
}

{
    my $h = %%;
    is $h.raku, '${}', 'a scalar assigned %% holds an itemized empty Hash';
    isa-ok (%%), Hash, '(%%) is a Hash';
}

{
    my %prov = Nil // %%;
    is %prov.elems, 0, '%% as the right-hand side of //';
}

{
    my %h = a => 1;
    is-deeply %%h, %h, '%%h is %h in hash context';
}

ok 6 %% 3, 'infix %% still means divisibility';
nok 7 %% 3, 'infix %% with a non-divisor';
ok 6%%3, 'infix %% without surrounding whitespace';

{
    my @a = 1, 2;
    is-deeply @@a, @a, '@@a is unchanged';
}
