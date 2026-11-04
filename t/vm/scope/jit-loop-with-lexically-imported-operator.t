use Test;

# A hot loop in a unit that has a `use` carries `LexImportBuiltinOp` prefixes
# on its built-in operators (ADR-8751 stage 2). The loop body must run the
# same natively compiled as interpreted: the prefix runs the operator with the
# lexically imported user candidates when a block binds them, and leaves it as
# the built-in otherwise.
plan 6;

use lib 't/lib';

# Unit-level `use` whose imports bind no operator: the operators stay built in.

my $t = 0;
for ^3000 { $t += $_ * 2 }
is $t, 8997000, 'built-in += and * in a hot loop after a use';

my $s = 0;
my $i = 0;
while $i < 3000 { $s = $s + $i - 1; $i++ }
is $s, 4495500, 'built-in + and - in a hot while loop';

# A block whose `use` binds operator candidates: the loop inside it must
# apply them on every iteration, the hot ones included.
{
    use UseInBlockBuiltinOpLexical;
    my $p = Pt.new(x => 1);
    my $acc = Pt.new(x => 0);
    for ^3000 { $acc = $acc + $p }
    is $acc.gist, 'Pt(3000)', 'a user candidate of + applies in every iteration';

    my $n = 0;
    for ^3000 { $n += 1 }
    is $n, 3000, 'the built-in still answers what no candidate takes';

    my $m = 0;
    for ^3000 { $m = $m + (5 - 2) }
    is $m, 309000, 'a candidate that defers to the built-in with callsame';
}

# And outside the block the same loop sees no candidate.
my $o = 0;
for ^3000 { $o = $o + (5 - 2) }
is $o, 9000, 'outside the block the built-in alone';
