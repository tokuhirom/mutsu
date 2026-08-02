use lib 't/lib';
use Test;
use MONKEY-SEE-NO-EVAL;

plan 3;

my $closure = EVAL q:to/CODE/;
    use EvalImportedClosure;
    sub (Int $value) { eval-imported-double($value) }
    CODE

is $closure(7), 14,
    'closure returned from EVAL retains the imported sub it calls';

my $factory = EVAL q:to/CODE/;
    use EvalImportedClosure;
    sub () { sub (Int $value) { eval-imported-double($value) } }
    CODE

is $factory()(9), 18,
    'an escaping closure pins imports used by a nested closure created later';

dies-ok { EVAL 'eval-imported-double(7)' },
    'the imported bare name does not leak out of the EVAL scope';
