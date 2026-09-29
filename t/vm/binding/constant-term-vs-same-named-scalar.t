use Test;
use lib 't/lib';

# A sigil-less `constant b` declares the TERM `b`; `my $b` / a parameter `$b`
# declares the variable `$b`. They are different symbols, so neither can see or
# shadow the other (#9962). mutsu stores a scalar sigil-stripped, and the
# constant used to share that key: the later binding won, and a bare `b` read
# whichever it was. The EC distribution's ed25519 is the shape that broke:
#
#     constant b = 256;
#     multi method new(blob8 $b where $b == b div 8) { ... }

plan 16;

constant b = 256;

{
    my @seen;
    sub g(Int $b where { @seen.push(b); True }) { @seen.push(b); $b }
    is g(5), 5, 'the parameter $b still binds its argument';
    is-deeply @seen, [256, 256],
        'a parameter $b shadows neither the term in its `where` block nor in the body';
}

{
    my $b = 7;
    is b, 256, 'a later `my $b` does not shadow the constant term';
    is $b, 7, 'and the scalar keeps its own value';
    is b + $b, 263, 'both are usable side by side';
}

{
    my $b = 9;
    is (EVAL 'b'), 256, 'EVAL sees the term, not the same-named scalar';
    is ::('b'), 256, 'indirect lookup of the term';
    is OUTER::<b>, 256, 'OUTER::<b> is the term';
    is MY::<$b>, 9, 'MY::<$b> is the scalar';
}

class Point {
    multi method new(blob8 $b where $b == b div 8) { 'blob' }
    multi method new(Int $n) { 'int' }
}
is Point.new(blob8.new(0 xx 32)), 'blob',
    'a `where` clause naming both the parameter and the constant (EC ed25519)';
is Point.new(3), 'int', 'the other candidate is unaffected';

{
    constant c = now;
    sub h($c) { c }
    isa-ok h(1), Instant, 'a non-foldable constant is read from its own slot, not the parameter';
}

dies-ok { EVAL 'b = 3' }, 'assigning to the constant term still dies';
is b, 256, 'and leaves the constant unchanged';

# Imported: `constant b is export` is a term in the importing scope too.
use ConstantTermExporter;
{
    my $ctx-term = 'scalar';
    is ctx-term, 'term', 'an imported constant term is not shadowed by a same-named scalar';
    is $ctx-term, 'scalar', 'and the scalar is not clobbered by the import';
}
