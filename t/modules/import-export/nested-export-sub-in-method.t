use v6;
use Test;
use lib 't/lib-nested-export-in-method';
use NestedExportInMethod;

# An `is export` routine nested in a method body is exported at compile time
# (before any invocation) and reads `self` and the lexicals of the latest
# invocation of that method (mutsu#14870, Pakku::Log's `multi sub log`).
plan 6;

NestedExportInMethod.new(:v(5), :bar<x>);
is nem-log('a'), 'a:5:10', 'exported multi sub sees the attribute and a lexical of BUILD';
is nem-log('b'), 'b:5:x', 'self and a second attribute resolve through the latest invocation';
is nem-log('a', :opt), 'a:5:10', 'a named argument is accepted by the slurpy';

NestedExportInMethod.new(:v(7), :bar<y>);
is nem-log('a'), 'a:7:14', 'the latest invocation wins';
is nem-log('b'), 'b:7:y', 'self follows the latest invocation';

sub caller-with-self {
  my class Other { has $.v = 99; method run { nem-log('a') } }
  Other.new.run
}
is caller-with-self(), 'a:7:14', "a caller's own self does not replace the sub's";
