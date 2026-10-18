use Test;

# `Any`'s keyed mutators on an undefined binding are rows (ADR-11276 §9.172):
# `ASSIGN-KEY` and `BIND-KEY` vivify the binding to a `Hash` (held in the
# `Scalar` of a `$` variable) and store through `Hash`'s row; `DELETE-KEY`
# answers `Nil` and leaves the receiver undefined. `AT-KEY` takes the
# ordinary rows. Expected values are Rakudo's.

plan 18;

{
    my $x;
    is $x.ASSIGN-KEY('a', 1), 1, 'ASSIGN-KEY answers the value';
    is $x.raku, '${:a(1)}', '... and vivifies a Hash in the Scalar';
    my $m = 'ASSIGN-KEY';
    my $w;
    $w."$m"('k', 5);
    is $w.raku, '${:k(5)}', 'a run-time method name';
    our $o;
    $o.ASSIGN-KEY('p', 1);
    is $o.raku, '${:p(1)}', 'an our variable';
}
{
    my $y;
    my $v = 2;
    $y.BIND-KEY('b', $v);
    $v = 3;
    is $y.raku, '${:b(2)}', 'Any.BIND-KEY binds the value, not the variable';
    my %h;
    my $w = 1;
    %h.BIND-KEY('x', $w);
    $w = 5;
    is %h.raku, '{:x(5)}', 'Hash.BIND-KEY still binds the variable';
}
{
    my $z;
    is $z.DELETE-KEY('c').raku, 'Nil', 'DELETE-KEY on an undefined receiver is Nil';
    is $z.raku, 'Any', '... and leaves it undefined';
}

# AT-KEY through the ordinary rows.
{
    my %h = a => 1;
    is %h.AT-KEY('a'), 1, 'Hash.AT-KEY';
    is %h.AT-KEY('zz').raku, 'Any', 'a missing key';
    my Int %t;
    is %t.AT-KEY('q').raku, 'Int', 'a typed hash answers its value type';
    my %d is default(42);
    is %d.AT-KEY('q'), 42, 'is default';
    %h.AT-KEY('a') = 7;
    is %h<a>, 7, 'AT-KEY answers the element container';
    my %o{Any};
    %o{1} = 'one';
    is %o.AT-KEY(1), 'one', 'an object hash';
    my $s = set <a b>;
    ok $s.AT-KEY('a'), 'Set.AT-KEY';
    my $bh = <a a b>.BagHash;
    is $bh.AT-KEY('a'), 2, 'BagHash.AT-KEY';
    my $r = %h;
    is $r.AT-KEY('a'), 7, 'a hash in a Scalar';
    is (a => 1, b => 2).Map.AT-KEY('b'), 2, 'Map.AT-KEY';
}
