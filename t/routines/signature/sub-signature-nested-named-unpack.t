use Test;

# Named parts destructured inside a sub-signature (CSS::Specification::
# Compiler's `compile` multis over the CSS::Specification actions' trees).

plan 7;

# The trailing parens of a named part unpack its value, in a nested
# sub-signature as at the top level.
sub inner-unpack(@ (:o(@)! (',', *%term))) { %term }
is-deeply inner-unpack([:o([',', :rule<a>])]), %(rule => 'a'), 'nested :o(@)! (...) unpacks';

multi pick(@ (:o(@)! (',', *%t))) { 'comma' }
multi pick(@ (:o(@)! ($x, *%t))) { 'other' }
is pick([:o([','])]), 'comma', 'dispatch checks the nested trailing parens';
is pick([:o(['x'])]), 'other', 'and falls to the next candidate';

# An `@`/`%` part binds the aggregate, not the item container it came in.
my @x = 1, 2;
my %h = (:args(@x));
sub count-args(%s (:@args!)) { my $k = 0; $k++ for @args; $k }
is count-args(%h), 2, 'a hash element holding an array binds its elements';
sub keep-args(%s (:@args!)) { my $k = 0; $k++ for @args; @args }
is-deeply keep-args(%h), [1, 2], 'iterating it leaves it intact';

# `is copy` owns a copy.
my @y = 1, 2, 3;
my %g = (:args(@y));
sub pop-copy(%s (:@args! is copy)) { @args.pop; @args.elems }
is pop-copy(%g), 2, 'is copy part can be changed';
is @y.elems, 3, "the caller's array is untouched";
