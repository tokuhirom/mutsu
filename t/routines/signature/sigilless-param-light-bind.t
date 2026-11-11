use Test;

# A sigilless (`\x`) positional parameter binds its argument raw: a caller
# variable is aliased (writes reach it, later caller writes are seen), a value
# is a read-only term. Each case runs several times so the call goes through
# the cached light dispatch as well as its first (cold) resolution. Found via
# AttrX::Mooish / Monad, whose helpers all take `Mu \obj` (#17335, #17376).

plan 17;

sub set-it(\x, $v) { x = $v }
sub read-it(\x) { x }
sub bump(Mu \x) { x = x + 1 }
sub type-name(Mu \x) { x.^name }

{
    my $c = 1;
    bump($c) for ^3;
    is $c, 4, 'writes through a sigilless parameter reach the caller variable';
}

{
    my @log;
    my $c = 10;
    sub seen(\x, &change) { change(); x }
    @log.push: seen($c, { $c = $c + 1 }) for ^3;
    is @log.join(','), '11,12,13', 'a caller write made during the call is seen through the parameter';
}

{
    my @r;
    for ^3 { @r.push: (try { set-it(5, 6); 'ok' }) // $!.^name }
    is @r.join(','), 'X::Assignment::RO,X::Assignment::RO,X::Assignment::RO',
        'a literal argument binds a read-only term';
}

{
    sub via($x) { set-it($x, 9) }
    my @r;
    for ^3 { @r.push: (try { via(3); 'ok' }) // 'died' }
    is @r.join(','), 'died,died,died', 'a read-only caller parameter stays read-only';
}

{
    my \k = 7;
    my @r;
    for ^3 { @r.push: (try { set-it(k, 8); 'ok' }) // 'died' }
    is @r.join(','), 'died,died,died', 'a read-only sigilless caller term stays read-only';
    is k, 7, '... and keeps its value';
}

{
    class Foo { method me { type-name(self) } }
    my @r = (^3).map({ type-name(Foo) ~ '/' ~ Foo.new.me });
    is @r.join(','), 'Foo/Foo,Foo/Foo,Foo/Foo', 'type objects and self bind as values';
}

{
    my @a = 1, 2, 3;
    bump(@a[1]) for ^2;
    my %h = a => 1;
    bump(%h<a>) for ^2;
    is "@a[] %h<a>", '1 4 3 3', 'element lvalues are aliased';
}

{
    sub outer(\y) { bump(y) }
    my $c = 0;
    outer($c) for ^4;
    is $c, 4, 'a sigilless parameter passed on reaches the root container';
}

{
    my Int $c = 1;
    my @r;
    for ^2 { @r.push: (try { set-it($c, 'str'); 'ok' }) // 'died' }
    is @r.join(','), 'died,died', "the caller variable's constraint applies to writes";
    is $c, 1, '... and the variable is unchanged';
}

{
    sub typed(Int \x) { x * 2 }
    is (^3).map({ typed($_ + 1) }).join(','), '2,4,6', 'a typed sigilless parameter binds';
    my @r;
    my $s = 'a';
    for ^2 { @r.push: (try { typed($s) }) // $!.^name }
    is @r.join(','), 'X::TypeCheck::Binding::Parameter,X::TypeCheck::Binding::Parameter',
        '... and checks its constraint';
}

{
    sub fact(\n) { n <= 1 ?? 1 !! n * fact(n - 1) }
    is fact(10), 3628800, 'recursion through a sigilless parameter';
}

{
    my $c = 5;
    is (^3).map({ read-it($c) }).join(','), '5,5,5', 'repeated reads of an aliased variable';
}

{
    my ($x, $y) = 3, 3;
    sub same(\p, \q) { p =:= q }
    sub same-rw($p is rw, $q is rw) { $p =:= $q }
    is (^3).map({ same($x, $x) ~ same($x, $y) }).join(','), 'TrueFalse,TrueFalse,TrueFalse',
        '=:= of two sigilless parameters bound to one variable';
    is (^3).map({ same-rw($x, $x) ~ same-rw($x, $y) }).join(','), 'TrueFalse,TrueFalse,TrueFalse',
        '=:= of two is rw parameters bound to one variable';
}
