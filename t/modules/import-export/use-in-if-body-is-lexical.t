use Test;
use lib 't/lib';

# A `use` in the body of an `if` is lexical to that body, like one in a bare
# block: a routine written outside it does not see the import while the body
# runs, and the import is gone after it (#15698). Checked against rakudo.
plan 5;

sub outside { (try ::('BgeType').hi) // 'none' }
my $t = 1;

if $t {
    use BlockGlobalExportType;
    sub inside { BgeType.hi }
    is inside(), 'bge', 'a sub written in the body sees the import';
    is outside(), 'none', 'a sub written outside the block does not';
}
is outside(), 'none', 'the import is gone after the if body';

if !$t { say 'no' } else {
    use BlockGlobalExportType;
    is BgeType.hi, 'bge', 'the else body imports lexically too';
}
is outside(), 'none', 'and it is gone after the else body';
