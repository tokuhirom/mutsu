use Test;

# `CALLER::OUTER::` and `CALLER::UNIT::` read the lexical scopes around the
# calling frame (ADR-12529 amendment A1, #18787). Expected values checked
# against rakudo.

plan 15;

# A block-declared sub's lexical outer is its declaring block, not the
# shadowing block it is called from.
{
    sub f1($f) { my $x is dynamic = 100; $f() }
    my $x = 102;
    {
        my $x = 104;
        is f1({ $CALLER::OUTER::x }), 102, 'CALLER::OUTER:: is the declaring block';
        is $x, 104, 'the shadowing block keeps its own binding';
    }
}

# A lexical the declaring block binds after the sub is still in its pad.
{
    sub g($f) { $f() }
    my $y = 5;
    is g({ $CALLER::OUTER::y }), 5, 'a later declaration of the scope';
}

# Each activation of the block has its own pad.
my @seen;
for 1, 2 -> $i {
    my $v = $i * 10;
    sub h($f) { $f() }
    @seen.push: h({ $CALLER::OUTER::v });
}
is-deeply @seen, [10, 20], 'each activation sees its own lexical';

# A write after the declaration is seen: the capture shares the variable.
{
    my $w = 1;
    sub ww($f) { $f() }
    $w = 2;
    is ww({ $CALLER::OUTER::w }), 2, 'a later write is seen';
}

# A sub declared in a routine body: its outer is that call of the routine.
sub outer-r() { my $q = 9; sub inner($f) { $f() }; inner({ $CALLER::OUTER::q }) }
is outer-r(), 9, 'a sub declared in a routine';
sub deep($n) {
    my $d = $n;
    sub dd($f) { $f() }
    $n > 0 ?? deep($n - 1) !! dd({ $CALLER::OUTER::d })
}
is deep(2), 0, 'the innermost activation of a recursive routine';

# A pointy block created in an EVAL'd unit's mainline: its UNIT is that
# EVAL unit, not the program's.
my $x110 = 110;
{
    my $x110 = 111;
    my $unit = 'UNIT';
    my $f = EVAL('my $x110 is dynamic = 113; -> $fn { my $x110 is dynamic = 114; $fn() }');
    is $f({ $CALLER::UNIT::x110 }), 113, 'CALLER::UNIT:: is the EVAL unit';
    is $f({ $CALLER::($unit)::x110 }), 113, 'CALLER::($unit):: is the EVAL unit';
}
{
    my $g = EVAL('my $u is dynamic = 7; -> $fn { $fn() }');
    is $g({ $CALLER::UNIT::u }), 7, 'an EVAL unit lexical the pointy does not redeclare';
    is $x110, 110, 'the program scope is untouched';
}

# A write the EVAL'd unit makes after creating the closure is seen (#19009),
# also when a nested block of the unit redeclares the name.
{
    my $h = EVAL('my $w is dynamic = 1; my $g = -> $fn { $fn() }; $w = 2; $g');
    is $h({ $CALLER::UNIT::w }), 2, 'a later write of the EVAL unit';
    my $k = EVAL('my $s is dynamic = 1; { my $s = 5; }; my $g = -> $fn { $fn() }; $s = 2; $g');
    is $k({ $CALLER::UNIT::s }), 2, 'a later write of a name a nested block redeclares';
}

# The unit's aggregates stay themselves: recording them for `UNIT::` does not
# wrap them in a scalar cell, so an in-place write the unit makes still lands.
{
    is EVAL('my %h; %h.classify-list({ $_ % 2 }, 1..6); %h.keys.sort.join(",")'),
        '0,1', 'an EVAL unit hash a closure argument fills in place';
    my $p = EVAL('my @l = 1; my $g = -> $fn { $fn() }; @l.push: 2; $g');
    is $p({ @CALLER::UNIT::l.elems }), 1, 'an EVAL unit array';
}
