use v6;
use Test;

plan 6;

# A class `is Range` inherits Range's own private predicate methods
# (`excludes-min`, `excludes-max`, `infinite`, `is-int`). Rakudo's `.bless`
# never actually populates Range's own private attributes for a subclass, so
# these always answer their class-declared default (False) unless the
# subclass overrides them -- ordinary inherited-method dispatch, which must
# win over `handles`/`FALLBACK` interception (tokuhirom/mutsu#8807).

class Bare is Range { }
my $bare = Bare.bless;
is $bare.excludes-min, False, 'excludes-min defaults False with no FALLBACK/handles';
is $bare.excludes-max, False, 'excludes-max defaults False with no FALLBACK/handles';
is $bare.infinite, False, 'infinite defaults False with no FALLBACK/handles';
is $bare.is-int, False, 'is-int defaults False with no FALLBACK/handles';

# The Math::Interval shape: `handles` delegates a specific list, and any
# Range-native method NOT on that list must still resolve normally instead
# of reaching FALLBACK.
class Interval is Range {
    has Range $!range is built handles <min max>;
    method FALLBACK($name) { die "FALLBACK: no method $name" }
}
my $iv = Interval.bless(range => (1..5));
is $iv.excludes-min, False, 'excludes-min resolves normally, not via FALLBACK';

# An explicit override still wins over the inherited default.
class Overridden is Range {
    method excludes-min { True }
}
is Overridden.bless.excludes-min, True, 'a user override beats the inherited default';
