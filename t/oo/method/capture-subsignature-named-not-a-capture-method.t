use v6;
use Test;

plan 8;

# A named parameter of a capture's sub-signature that the call did not pass is
# absent; it is not read from a method of the Capture that happens to share its
# name (`Capture.cache` is a List, so `Bool :$cache` failed its type check).
sub with-cache(|c (:$base, Bool :$cache = True)) { $cache }
is with-cache(:base(1)), True, 'unpassed Bool :$cache takes its default';
is with-cache(:base(1), :cache(False)), False, 'a passed :$cache is bound';

sub without-default(|c (Bool :$cache)) { $cache.WHAT.^name }
is without-default(), 'Bool', 'unpassed Bool :$cache without default is the Bool type object';

sub other-names(|c (Int :$list, Str :$pairs, Int :$elems)) { ($list, $pairs, $elems).map({ .defined ?? 1 !! 0 }).join }
is other-names(), '000', 'names of other Capture methods stay absent';
is other-names(:list(3), :elems(2)), '101', 'and are bound when passed';

sub positional-and-named(|c (Int $p, Int :$y)) { "$p/{$y // 'none'}" }
is positional-and-named(1), '1/none', 'positional with an absent named';
is positional-and-named(1, :y(2)), '1/2', 'positional with a passed named';

my %h = :x(1);
sub from-hash(%h (:$x, Bool :$cache = True)) { "$x/$cache" }
is from-hash(%h), '1/True', 'a Hash unpack ignores Hash methods as well';
