use v6;
use Test;

# `UNIT::` names the compilation unit's OUTERMOST lexical scope — the file
# mainline. Where `OUTER::` steps one scope out, `UNIT::` jumps to the top, so a
# name shadowed at every inner level still reads its mainline binding (roast
# 6.c/S02-names/pseudo-6c.t UNIT block, tests 140/141). Both the direct
# `$UNIT::x` spelling and the indirect `$::('UNIT')::x` resolve through the same
# lexical scope-chain walk that backs `OUTER::`.

plan 6;

my $x110 = 110;
{
    my $x110 = 111;
    my $unit = 'UNIT';
    is $UNIT::x110, 110, '$UNIT:: reads the mainline binding past one shadow';
    is $::($unit)::x110, 110, '::("UNIT") reads the mainline binding (indirect)';

    {
        my $x110 = 112;
        is $UNIT::x110, 110, '$UNIT:: jumps to the top past two shadows';
        is $OUTER::x110, 111, '... while $OUTER:: still steps just one scope out';
        is $::($unit)::x110, 110, '::("UNIT") jumps to the top (indirect)';
    }
}

# A name the mainline never declares is undefined through UNIT::.
{
    my $only-inner = 7;
    nok $UNIT::only-inner.defined, '$UNIT:: of an inner-only name is undefined';
}
