use Test;

plan 11;

sub proxy-for($initial) {
    my $value = $initial;
    return-rw Proxy.new(
        FETCH => method () { $value },
        STORE => method ($new) { $value = $new },
    );
}

{
    my Int $bound := proxy-for(1);
    is $bound, 1, 'a typed scalar checks the Proxy FETCH value';
    is $bound.VAR.^name, 'Proxy', 'the binding retains the Proxy container';
    $bound = 12;
    is $bound, 12, 'assignment to the bound scalar calls Proxy STORE';
}

{
    my Str $bound := proxy-for('before');
    is $bound.VAR.^name, 'Proxy', 'a matching Str FETCH also retains the Proxy';
    $bound = 'after';
    is $bound, 'after', 'the Str Proxy stays writable';
}

{
    my $source := proxy-for(7);
    my Int $bound := $source;
    is $bound, 7, 'a typed bind through a Proxy-bound variable checks FETCH';
    is $bound.VAR.^name, 'Proxy', 'the VarRef source keeps its Proxy container';
}

{
    my $fetches = 0;
    my Int $bound := Proxy.new(
        FETCH => method () { $fetches++; 3 },
        STORE => method ($new) { },
    );
    is $fetches, 1, 'a typed bind FETCHes once';
}

throws-like { my Int $bound := proxy-for('wrong') },
    X::TypeCheck::Binding, 'a mismatching FETCH fails the binding';
throws-like { my Str $bound := proxy-for(1) },
    X::TypeCheck::Binding, 'a Str bind does not coerce a mismatching FETCH';
throws-like { my Int $bound := proxy-for(Nil) },
    X::TypeCheck::Binding, 'a typed bind rejects a Proxy that FETCHes Nil';
