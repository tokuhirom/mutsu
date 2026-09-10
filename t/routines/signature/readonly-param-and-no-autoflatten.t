use Test;

plan 12;

# --- a read-only parameter (the default) cannot be modified in place ---
{
    sub pre-inc($n)  { ++$n }
    sub pre-dec($n)  { --$n }
    sub post-inc($n) { $n++ }
    sub assign($n)   { $n = 5 }

    dies-ok { pre-inc(3)  }, 'prefix ++ on a read-only param dies';
    dies-ok { pre-dec(3)  }, 'prefix -- on a read-only param dies';
    dies-ok { post-inc(3) }, 'postfix ++ on a read-only param dies';
    dies-ok { assign(3)   }, 'assignment to a read-only param dies';

    throws-like { pre-inc(3) }, X::Multi::NoMatch,
        'prefix ++ throws X::Multi::NoMatch';
}

# --- `is rw` / `is copy` params remain modifiable ---
{
    sub rw-inc($n is rw)     { ++$n }
    sub copy-inc($n is copy) { ++$n; $n }
    my $x = 3;
    rw-inc($x);
    is $x, 4, 'is rw param: prefix ++ modifies the caller';
    my $y = 3;
    is copy-inc($y), 4, 'is copy param: prefix ++ works on the copy';
    is $y, 3, 'is copy param: caller left unchanged';
}

# --- a bare @array argument is ONE positional; it does NOT auto-flatten ---
{
    sub three($x, $y, $z) { "$x|$y|$z" }
    my @te = <a b c>;

    dies-ok { three(@te) },
        'a single @array does not flatten to fill 3 scalar params';
    is three(|@te), 'a|b|c',
        'an explicit slip |@array does flatten';

    sub one($x) { $x.elems }
    is one(@te), 3, 'a single @array binds whole to one param';

    sub two($x, $y) { "$x.elems()|$y" }
    is two(@te, 9), '3|9', '@array binds to the first param, next arg to the second';
}
