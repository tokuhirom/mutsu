use v6;
use lib $?FILE.IO.parent.child('lib').Str;
use Test;

# `unit class EnumHolder; our enum EHFormat <... EhLists>;` — when that class
# is first loaded inside ANOTHER module's package block, the block exit drops
# the bare enum-member env keys (only `::`-qualified keys survive). A bare
# member read inside the declaring class's own methods (URI::Query's
# `:$hash-format = Lists` default) must still resolve to the enum value, not
# fall back to a Str.

use EnumHolderWrap;
use EnumHolder;

plan 4;

ok EnumHolderWrap::present(), 'wrapper module loaded';

my $e = EnumHolder.new("x");
nok $e.fmt ~~ Str, 'enum-typed attribute got the enum value, not the Str fallback';
is $e.fmt, EnumHolder::EhLists, 'bare member in method default resolved to the right member';
is EnumHolder.bare-member, EnumHolder::EhMixed, 'bare member read in a method body resolves';

done-testing;
