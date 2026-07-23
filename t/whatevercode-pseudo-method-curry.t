use v6;
use Test;

plan 10;

# Rakudo's non-currying pseudo-methods on Whatever/WhateverCode are exactly
# WHAT/WHO/HOW/WHERE/DEFINITE/VAR — `.WHICH` and `.WHY` DO curry.

is (* + 1).WHAT.gist, '(WhateverCode)', '.WHAT on a WhateverCode does not curry';
is *.WHAT.gist, '(Whatever)', '.WHAT on bare * does not curry';
ok (* + 1).DEFINITE, '.DEFINITE does not curry';
is (* + 1).WHERE.^name, 'Int', '.WHERE does not curry and is an Int';
is (* + 1).VAR.^name, 'WhateverCode', '.VAR does not curry';
is (* + 1).WHO.^name, 'Stash', '.WHO does not curry';

is (*.WHICH).("a").gist.substr(0, 4), 'Str|', '*.WHICH curries into a WhateverCode';
my $which = (* + 1).WHICH;
is $which.^name, 'WhateverCode', '.WHICH on a WhateverCode curries';
my $why = (* + 1).WHY;
is $why.^name, 'WhateverCode', '.WHY curries';

my $w = * + 1;
is $w.WHAT.gist, '(WhateverCode)', '.WHAT on a WhateverCode in a variable';

done-testing;
