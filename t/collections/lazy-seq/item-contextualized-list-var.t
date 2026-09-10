use Test;

plan 3;

is $(4, 5).VAR.^name, 'Scalar', 'itemized list VAR reports Scalar';
ok $(1, 2, 3).VAR ~~ Scalar, 'itemized list VAR is a Scalar';
nok (1, 2, 3).VAR ~~ Scalar, 'plain list VAR is not a Scalar';
