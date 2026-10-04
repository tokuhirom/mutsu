use Test;

# The key of an enum declared in a class body is not a term outside that
# body (rakudo: "Undeclared routine"), while the class's own methods and the
# qualified name still reach it (#11719).

plan 7;

class CC {
    enum E <bar>;
    method m { bar }
}

throws-like { EVAL 'bar' }, X::Undeclared::Symbols,
    'a class-body enum key is undeclared outside the class';
{
    # The same read compiled into a program, not judged by EVAL's own check.
    my $proc = run $*EXECUTABLE, '-e', 'class CC { enum E <bar> }; print bar',
        :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ok $out eq '' && $err.contains('Undeclared routine'),
        'a program reading the key outside the class dies as undeclared';
}
is CC.m, 'bar', "the class's own method sees the key";
is CC::bar, 'bar', 'the qualified key resolves';

class C2 { enum F <array cos> }
my array[int] $a;
is $a.WHAT.^name, 'array[int]', 'a key named like a native type does not hide it';
is cos(0), 1, 'a key named like a builtin routine does not hide it';

{
    my enum G <bar>;
    is bar, 'bar', 'a later declaration of the name in another scope wins';
}
