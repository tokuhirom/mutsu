use Test;

plan 8;

ok %(a => 42) ~~ :(Int :$a), 'Hash smart-matches against named signature';
ok [1, 2] ~~ :($a, $b), 'Array smart-matches against positional signature';

{
    sub f($ = 123) { }
    ok \() ~~ &f.signature, 'Capture smart-matches signature with default';
}

ok set(<a b>) ~~ :(:$a where .so, :$b where .so),
    'Set smart-matches signature with where constraints';
ok (1, 2, :42c) ~~ :($a where 1, $b where 2, :$c where 42),
    'List Pair contributes named args during signature smart-match';

ok <1/2> ~~ :(Int :$numerator where 1, Int :$denominator where 2),
    'Rat exposes numerator/denominator for signature smart-match';
nok <1/2> ~~ :(Int :$numerator where 3, Int :$denominator where 2),
    'Rat fails signature smart-match when where constraint fails';

nok 42 ~~ :(Int), 'Non-capture scalar does not smart-match signature';
