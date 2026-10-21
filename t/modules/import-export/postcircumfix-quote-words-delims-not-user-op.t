use Test;
plan 3;

class C { method AT-KEY($k) { "k:$k" } }
multi sub postcircumfix:«<< >>»(C:D $c, $k) { "ww:$k" }
my $c = C.new;

is $c<<foo>>, 'k:foo', 'a declared postcircumfix:<< >> is not called for <<key>>';
is $c<foo>, 'k:foo', '<key> is unaffected';
is $c«foo», 'k:foo', 'the « » spelling is unaffected';
