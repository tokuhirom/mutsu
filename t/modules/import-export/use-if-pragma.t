use Test;

plan 4;

# The `if` pragma: `use Foo:if(EXPR)` loads Foo only when EXPR is true at
# runtime. `use if;` itself is a no-op that just enables the adverb.
# (Crypt::Random uses this to pick a platform backend:
#  `use Crypt::Random::Win:if($*DISTRO.is-win)`.)

use if;
pass "use if; is a no-op pragma";

# False condition: the module is NOT loaded, even if it does not exist.
use Totally::Nonexistent::Module:if(False);
pass "use Mod:if(False) skips the load entirely";

# True condition: the module IS loaded and its exports are available.
my $cond = True;
use List::Util:if(False);   # not loaded
use Test:if($cond);         # already loaded; just exercises the true branch
pass "use Mod:if(True) takes the load branch";

# Negated platform-style condition (true on non-Windows).
use Totally::Other::Missing:if($*DISTRO.is-win);
pass "use Mod:if(\$*DISTRO.is-win) skips on non-Windows";
