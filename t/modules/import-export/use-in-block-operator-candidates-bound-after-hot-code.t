use Test;

# ADR-8751 stage 2, #18211: a built-in operator op inside a block with a
# `use` skips its import probe while no import set in the process binds an
# operator. Code that ran (and went hot enough to be JIT-compiled) before the
# first such set existed must still see the candidates once one does. Checked
# against rakudo, where the `use` binds at compile time and the Int calls are
# answered by the built-in either way.
plan 4;

use lib 't/lib';

{
    sub add($a, $b) { $a + $b }
    sub same($a, $b) { $a eq $b }
    my $sum = 0;
    my $eqs = 0;
    for ^3000 {
        $sum = add($sum, 1);
        $eqs++ if same('x', 'x');
    }
    is $sum, 3000, 'the built-in answers before the import set is bound';
    is $eqs, 3000, 'so does a string comparison';

    use UseInBlockBuiltinOpLexical;

    is add(Pt.new(x => 1), Pt.new(x => 2)).gist, 'Pt(3)',
        'the same hot code sees the imported candidate afterwards';
    is add(4, 5), 9, 'and still falls back to the built-in';
}
