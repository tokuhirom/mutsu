use Test;

plan 10;

{
    package Foo {
        constant \term:<ℵ₀> = Inf;
        is ℵ₀, Inf, "constant term declarator resolves in package scope";
    }
    dies-ok { EVAL "ℵ₀" }, "package-scoped constant term does not leak";
    is Foo::term:<ℵ₀>, Inf, "package-qualified term constant lookup works";
}

{
    {
        my \term:<ℵ₀> = Inf;
        is ℵ₀, Inf, "lexical constant term resolves in scope";
        is EVAL('ℵ₀'), Inf, "lexical constant term resolves in EVAL";
    }
    dies-ok { EVAL "ℵ₀" }, "lexical constant term does not leak";
}

{
    my $a = 0;
    sub term:<•> { $a++ };
    is •, 0, "sub term declarator can define symbolic term";
    is •, 1, "sub term declarator evaluates each use";
}

{
    my $a = 0;
    my &term:<•> = { $a++ };
    is •, 0, "code variable term declarator can define symbolic term";
    is •, 1, "code variable term declarator evaluates each use";
}
