use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 3;

# `:name<>` (empty angle brackets) means `name => ()` (an empty list), not a
# null string. Raku emits a compile-time "Potential difficulties" warning that
# suggests the `:name('')` / `:name()` forms.
# See roast/S32-exceptions/misc.t (old-issue-tracker #3701).
is_run ｢my $ = :WorryFoo<>; print "pass"｣,
    %(:out<pass>, :err{ .contains: 'use :WorryFoo' }),
    'empty-angle colonpair warns at compile time';

# The lexical `no worries` pragma disables compiler warnings for its scope only;
# warnings outside that scope are still emitted.
is_run ｢
    my $ = :WorryFoo<>; { no worries; my $ = :WorryBar<> }; my $ = :WorryBer<>;
    print "pass"
｣, %(:out<pass>, :err{
    .contains: 'use :WorryFoo' & 'use :WorryBer' & none 'use :WorryBar'
}), 'lexical worries pragma disables compiler warnings';

# `:name<>` parses to a Pair whose key is the name.
{
    my $p = :foo<>;
    is $p.key, 'foo', ':foo<> produces a Pair keyed by the name';
}
