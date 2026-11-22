use Test;

# A `Nil` assigned through an untyped name bound to a typed container resets
# that container to its own type object (#18755).

plan 6;

class C { has Int $!x = 1; method m { $!x }; method acc is rw { $!x } }
{
    my $c = C.new;
    my $r := $c.acc;
    $r = Nil;
    is $c.m.raku, 'Int', 'is rw method container: Nil resets to the type object';
}

class D { has Int $.x is rw = 1 }
{
    my $d = D.new;
    my $s := $d.x;
    $s = Nil;
    is $d.x.raku, 'Int', 'public rw accessor container: Nil resets to the type object';
    $s = 5;
    is $d.x, 5, 'a later store still reaches the attribute';
}

class E { has Int $.x is rw is default(42) = 1 }
{
    my $e = E.new;
    my $s := $e.x;
    $s = Nil;
    is $e.x, 42, 'is default wins over the type object';
}

{
    my Int $x = 1;
    my $y := $x;
    $y = Nil;
    is $x.raku, 'Int', 'a bound typed lexical resets to its type object';
}

{
    my $u = 1;
    my $v := $u;
    $v = Nil;
    is $u.raku, 'Any', 'an untyped container still resets to Any';
}
