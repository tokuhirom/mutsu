use Test;

plan 9;

# The `if` pragma (the bundled `if` distribution, ADR-0098): `use Foo:if(EXPR)`
# loads Foo only when EXPR is true. The adverb is NOT built into the language —
# it is provided by the module, whose `sub EXPORT` mixes an actions role
# overriding `statement-control:sym<use>` into the compiling unit's MAIN slang.
# (Crypt::Random uses it to pick a platform backend:
#  `use Crypt::Random::Win:if($*DISTRO.is-win)`.)

# The module's EXPORT branches on this to choose between mixing a role into the
# legacy frontend's `$*W` and registering a slang through `$*LANG`. mutsu offers
# the `$*LANG` surface, so it answers the way the RakuAST frontend does. (This
# is the one assertion here that is mutsu's own: rakudo's default frontend is
# still the legacy one and answers `True`, `RAKUDO_RAKUAST=1` answers `False`.)
is Raku.legacy, False, 'Raku.legacy is False';

# Nothing below the pragma has run yet, so in THIS unit the adverb is still
# inert. An EVAL string is its own compilation unit and never inherits the
# pragma, which makes it the way to exercise both sides.
eval-dies-ok 'use Totally::Nonexistent::NoPragma:if(False)',
    'without the pragma :if(False) is inert, so the module is still loaded';
eval-lives-ok 'use if; use Totally::Nonexistent::WithPragma:if(False)',
    'with the pragma :if(False) skips the load entirely';

use if;
pass 'use if; loads the bundled pragma module';

# False condition: the module is NOT loaded, even if it does not exist.
use Totally::Nonexistent::Module:if(False);
pass 'use Mod:if(False) skips the load entirely';

# True condition: the module IS loaded and its exports are available. The
# condition is a `constant` because rakudo evaluates the adverb at BEGIN time,
# where a `my $cond = True` assigned in the mainline has not run yet.
constant COND = True;
use List::Util:if(False);   # not loaded
use Test:if(COND);          # already loaded; just exercises the true branch
pass 'use Mod:if(True) takes the load branch';
ok &ok.defined, 'the true branch still imports';

# Negated platform-style condition (true on non-Windows).
use Totally::Other::Missing:if($*DISTRO.is-win);
pass 'use Mod:if($*DISTRO.is-win) skips on non-Windows';

# A version comparison is the shape the ecosystem actually uses
# (`use Net::BGP::Conversions-Pre201812:if($*PERL.compiler.version < v2018.12)`).
use Totally::Other::Ancient:if($*RAKU.version ~~ v7);
pass 'use Mod:if($*RAKU.version ~~ v7) skips';
