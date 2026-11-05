use Test;

plan 11;

# A WhateverCode (`*+1`, `*.abs`) renders as the source it was written as for
# .gist / .raku / .perl (rakudo's RakuAST frontend; #16654), not as a generic
# closure form or an empty string.
my $c = * + 1;
is $c.raku, '* + 1', 'WhateverCode .raku';
is $c.perl, '* + 1', 'WhateverCode .perl';
is $c.gist, '* + 1', 'WhateverCode .gist';

my $d = *.abs;
is $d.raku, '*.abs', 'method-call WhateverCode .raku';

# In a list / array .raku and .gist, each WhateverCode element renders too.
is (* + 1, * - 1).raku, '(* + 1, * - 1)',
    'WhateverCode elements in a list .raku';
my @a = * + 1, * - 1;
is @a.raku, '[* + 1, * - 1]', 'WhateverCode elements in an array .raku';
is @a.gist, '[* + 1 * - 1]', 'WhateverCode elements in an array .gist';

# As a pair / hash value.
is (a => * + 1).raku, ':a(* + 1)', 'WhateverCode as a pair value .raku';

# The WhateverCode still works as a callable (the rendering change is cosmetic).
is $c(5), 6, 'WhateverCode is still callable after rendering';
is (* + 1)(10), 11, 'inline WhateverCode call works';

# A regular block is NOT a WhateverCode and must not render as one.
my $b = { $_ + 1 };
isnt $b.raku, 'WhateverCode.new', 'a plain block does not render as WhateverCode';
