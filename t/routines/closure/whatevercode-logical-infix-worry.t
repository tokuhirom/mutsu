use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# A WhateverCode as the first operand of a short-circuiting logical infix
# does not curry the whole expression: rakudo warns at compile time that it is
# always true and defined (#18139).

plan 6;

is_run ｢my @a = 1, 2, 7; print @a.first((* > 1 and * < 5))｣,
    %(:out<1>, :err{ .contains: 'WhateverCode tested first by and does not curry the and expression' }),
    'and';

is_run ｢my $b = (* > 1 || 3); my $c = (* > 1 xor 3); my $d = (* > 1 andthen 3); print 'ok'｣,
    %(:out<ok>, :err{
        .contains: 'tested first by || does not curry the || expression'
            & 'tested first by xor does not curry the xor expression'
            & 'tested first by andthen does not curry the andthen expression'
    }),
    '||, xor and andthen are named as written';

is_run ｢my $b = (* > 1 || 3); print 'ok'｣,
    %(:out<ok>, :err{ .contains: "and is always true and defined.\n    Try a block using \$_ instead if results are not as expected." }),
    'the advice';

is_run ｢my $m = (* min 3); my $w = (1 and * > 1); print 'ok'｣,
    %(:out<ok>, :err('')),
    'no worry for min or a WhateverCode second operand';

is_run ｢my $s = (* and 3); print 'ok'｣,
    %(:out<ok>, :err{ .contains: 'Whatever tested first by and does not curry the and expression' }),
    'a bare Whatever is named as such';

is_run ｢{ no worries; my $d = (* > 1 or 3); }; print 'ok'｣,
    %(:out<ok>, :err('')),
    'no worries silences it';
