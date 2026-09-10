use Test;

plan 11;

# A WhateverCode (`*+1`, `*.abs`) renders as `WhateverCode.new` for
# .gist / .raku / .perl, and via say (which gists), not as a generic
# closure form or an empty string.
my $c = * + 1;
is $c.raku, 'WhateverCode.new', 'WhateverCode .raku';
is $c.perl, 'WhateverCode.new', 'WhateverCode .perl';
is $c.gist, 'WhateverCode.new', 'WhateverCode .gist';

my $d = *.abs;
is $d.raku, 'WhateverCode.new', 'method-call WhateverCode .raku';

# In a list / array .raku and .gist, each WhateverCode element renders too.
is (* + 1, * - 1).raku, '(WhateverCode.new, WhateverCode.new)',
    'WhateverCode elements in a list .raku';
my @a = * + 1, * - 1;
is @a.raku, '[WhateverCode.new, WhateverCode.new]', 'WhateverCode elements in an array .raku';
is @a.gist, '[WhateverCode.new WhateverCode.new]', 'WhateverCode elements in an array .gist';

# As a pair / hash value.
is (a => * + 1).raku, ':a(WhateverCode.new)', 'WhateverCode as a pair value .raku';

# The WhateverCode still works as a callable (the rendering change is cosmetic).
is $c(5), 6, 'WhateverCode is still callable after rendering';
is (* + 1)(10), 11, 'inline WhateverCode call works';

# A regular block is NOT a WhateverCode and must not render as one.
my $b = { $_ + 1 };
isnt $b.raku, 'WhateverCode.new', 'a plain block does not render as WhateverCode';
