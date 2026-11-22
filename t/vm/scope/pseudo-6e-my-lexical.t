use v6.e.PREVIEW;
use Test;

# 6.e's MY:: is the innermost pad only, LEXICAL:: every lexical visible, and
# the root stash :: sees the pseudo-packages (#18742; rakudo 2026.11).

plan 21;

{
    my $x = 10;
    {
        my $y = 12;
        is $MY::y, 12, '$MY::y finds the pad\'s own variable';
        throws-like { $MY::x }, X::NoSuchSymbol, '$MY::x of an outer scope fails';
        throws-like { MY::.{'$x'} }, X::NoSuchSymbol, 'MY::.{} of an outer name fails';
        isa-ok MY::<$x>, Failure, 'a MY:: miss is a Failure';
    }
    my class A1 { our $pies = 14; method pies() { } }
    {
        throws-like { MY::A1 }, X::NoSuchSymbol, 'MY::classname of an outer scope fails';
        ok LEXICAL::.{'A1'}.^can('pies'), 'LEXICAL:: sees an outer scope\'s class';
    }
    throws-like { EVAL 'MY::A2' }, X::NoSuchSymbol, 'MY::Name of an unknown name fails';
    is $LEXICAL::A1::pies, 14, '$LEXICAL::A1::pies reads the lexical package';

    our $a18 = 19;
    $MY::a18 := $x;
    ok $a18 =:= $x, '$MY:: binding rebinds an our-alias';

    my $l = 22;
    is ::.<MY>.WHO.<$l>, 22, 'the root stash names MY';
    ok ::<OUTERS>:exists, 'the root stash sees pseudo-packages';
}

{
    my $x = 10;
    {
        my $y = 12;
        is $LEXICAL::y, 12, '$LEXICAL::y finds the shadow';
        is $LEXICAL::x, 10, '$LEXICAL::x finds the outer variable';
    }
    my $z;
    {
        $LEXICAL::z := $x;
        ok $z =:= $x, 'binding through $LEXICAL::z';
        my $y = 3;
        LEXICAL::.{'$z'} := $y;
        ok $z =:= $y, 'binding through LEXICAL::.{}';
    }
}

{
    my $x = 1;
    my $y = 2;
    {
        ::<$x> := $y;
        $y = 1.5;
        is $x, 1.5, 'binding through the root stash';
        ok $x =:= $y, '... shares the container';
    }
}

{
    my $z;
    sub callee { LEXICAL::.{'$*k'} }
    sub callee2($f is rw) { LEXICAL::.{'$*k'} := $f }
    {
        my $*k = 16;
        my $z = 17;
        is callee(), 16, 'LEXICAL::<$*k> searches the dynamic scope';
        callee2($z);
        ok $*k =:= $z, 'LEXICAL::<$*k> binds the dynamic variable';
    }
}

# A 6.e pseudo-stash fails on a miss; OUR:: is a real Stash and does not.
isa-ok { MY::<$nope> }(), Failure, 'a miss at a block\'s tail fails too';
is (OUR::<$nope>).^name, 'Any', 'OUR:: is no pseudo-stash';
