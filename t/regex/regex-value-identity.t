use Test;

# A regex is a `Code` object: every evaluation of a regex literal or an
# anonymous declarator term is a distinct value, while every alias of one
# value is the same object (#10670).

plan 23;

{
    my @r = (^2).map({ regex {a} });
    nok @r[0] === @r[1], 'two evaluations of an anonymous regex declarator are not ===';
    my @s = (^2).map({ /a/ });
    nok @s[0] === @s[1], 'two evaluations of a /.../ literal are not ===';
    my @t = (^2).map({ rx:i/a/ });
    nok @t[0] === @t[1], 'two evaluations of an adverbed rx literal are not ===';
    my @u = (^2).map({ token { a } });
    nok @u[0] === @u[1], 'two evaluations of an anonymous token are not ===';
}

{
    my $a = /a/;
    my $b = $a;
    ok $a === $b, 'an alias of a regex value is ===';
    is $a.WHICH, $b.WHICH, 'an alias of a regex value has the same WHICH';
    ok $a.WHICH.Str.starts-with('Regex|'), 'WHICH names the Regex type';
    nok /a/ === /a/, 'two textually identical literals are not ===';
    ok /a/ eqv /a/, 'eqv on two textually identical literals stays structural';
}

{
    my $x = rx:i/a/;
    my $y = $x;
    ok $x === $y, 'an alias of an adverbed regex is ===';
    is $x.WHICH, $y.WHICH, 'an alias of an adverbed regex has the same WHICH';
}

{
    my sub f { /a/ }
    nok f() === f(), 'each call returning a regex literal returns a new one';
    my @w = (^3).map({ /b/ }).map(*.WHICH);
    is @w.unique.elems, 3, 'every evaluation has its own WHICH';
}

{
    my $r = /a/;
    my %h{Any};
    %h{$r} = 1;
    is %h{$r}, 1, 'a regex value is an object-hash key by identity';
    nok %h{/a/}:exists, 'another evaluation of the same literal is a different key';
    ok 'xa' ~~ $r, 'a regex value still matches';
}

{
    my $r = rx:i/a/;
    $r.set_name('q');
    is $r.name, 'q', 'set_name renames an adverbed regex';
    my $alias = $r;
    is $alias.name, 'q', 'the name is seen through an alias';
    my $s = /a/;
    $s.set_name('p');
    is $s.name, 'p', 'set_name on a plain literal';
    my sub mk { /a/ }
    mk().set_name('z');
    is mk().name, '', 'renaming one evaluation does not leak into the next';
    my @t = (^2).map({ rx:i/b/ });
    @t[0].set_name('first');
    is @t[1].name, '', 'renaming one adverbed evaluation does not leak into another';
    (my $c = rx:s/a b/).set_name('c');
    is $c.name, 'c', 'set_name on a sigspace regex';
    my $tok = token { x };
    $tok.set_name('tk');
    is $tok.name, 'tk', 'set_name on an anonymous token';
}
