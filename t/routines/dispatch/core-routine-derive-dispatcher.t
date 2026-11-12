use Test;
use experimental :rakuast;

# A built-in routine is a dispatcher: `.clone.derive_dispatcher` gives one of
# its own, whose added candidates answer first and whose built-in answers the
# rest (#15371, CompUnit::Util's `push-lexical-multi`). A BEGIN-time
# `$*R.resolve-lexical-constant` finds the routine. Checked against rakudo.

plan 9;

class Foo { }

ok &postcircumfix:<{ }>.is_dispatcher, 'a built-in operator is a dispatcher';
ok &say.is_dispatcher, 'so is a built-in routine';

my $d := &postcircumfix:<{ }>.clone.derive_dispatcher;
ok $d.is_dispatcher, 'the derived routine is a dispatcher';
$d.add_dispatchee(sub (Foo:D $, Str:D $) { 'win' });
is $d(Foo.new, 'x'), 'win', 'an added candidate answers';
is $d({ a => 1 }, 'a'), 1, 'the built-in answers the rest';
is { b => 2 }<b>, 2, 'the built-in itself is unchanged';

BEGIN my $found = $*R.resolve-lexical-constant('&say').compile-time-value.name;
is $found, 'say', 'a BEGIN-time lookup finds a routine';
BEGIN my $only = $*R.resolve-lexical-constant('&say', :current-scope-only).defined;
nok $only, 'but not in the current scope alone';
BEGIN my $none = $*R.resolve-lexical-constant('&no-such-routine').defined;
nok $none, 'and nothing for an undeclared one';
