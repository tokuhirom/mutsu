use Test;

# A typed `my` in a `for` body, after an `@`-array initialised from the topic,
# must not outlive the loop (#13516): the block-entry hoist seeds the type
# object, and the loop exit has to take that seed away again.
plan 4;

for <a-b> { my @t = $_; my Str $nm = "q"; }
is (try EVAL('nm').raku) // 'undeclared', 'undeclared', 'typed my does not leak past for';

for <a-b> { my @t = $_; my $nm2 = "q"; }
is (try EVAL('nm2').raku) // 'undeclared', 'undeclared', 'untyped my does not leak past for';

for <x y> { my (Str $k, Int $v) = $_, 1; }
is (try EVAL('k').raku) // 'undeclared', 'undeclared', 'typed destructuring does not leak';

my Str $outer = "keep";
for <a> { my @t = $_; my Str $outer = "inner"; }
is $outer, "keep", 'a shadowing typed my restores the outer binding';
