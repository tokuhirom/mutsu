use Test;

plan 4;

my $a;
nok $a.VAR.dynamic, 'variables are not dynamic by default';

subtest 'dynamic-scope pragma without args' => {
    use dynamic-scope;
    my $b;
    ok $b.VAR.dynamic, 'my variables become dynamic in scope';
    my ($c, $d);
    ok $c.VAR.dynamic, 'list declaration variable (1) becomes dynamic';
    ok $d.VAR.dynamic, 'list declaration variable (2) becomes dynamic';
}

my $x;
nok $x.VAR.dynamic, 'dynamic-scope pragma is lexical';

subtest 'dynamic-scope pragma with args' => {
    use dynamic-scope <$e $g>;
    my $e;
    ok $e.VAR.dynamic, 'matching variable becomes dynamic';
    my $f;
    nok $f.VAR.dynamic, 'non-matching variable remains lexical';
    my ($g, $h);
    ok $g.VAR.dynamic, 'list declaration variable (1) can match';
    nok $h.VAR.dynamic, 'list declaration variable (2) can stay lexical';
}
