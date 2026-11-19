use Test;

# A wrapped Method object called as a value (`$obj.$m()`, `$m($obj)`) runs
# its wrappers around the owner's candidate, as an ordinary method call does
# (#18049).

plan 7;

class W { method m($x = 1) { "inner$x" } }
class S is W { method m($x = 1) { "sub$x" } }

my $wm = W.^find_method('m');
my $h = $wm.wrap(-> |c { 'w(' ~ callsame() ~ ')' });

is W.new.m, 'w(inner1)', 'an ordinary call runs the wrapper';
is W.new.$wm(), 'w(inner1)', '$obj.$m() runs the wrapper';
is $wm(W.new, 5), 'w(inner5)', '$m($obj, ...) runs the wrapper';
is S.new.$wm(), 'w(inner1)', "a subclass invocant still reaches the owner's candidate";

$wm.wrap(-> $self, |c { 'o[' ~ callwith($self, 9) ~ ']' });
is W.new.$wm(), 'o[w(inner9)]', 'two wrappers nest, outermost first';

$wm.unwrap($h);
is W.new.$wm(3), 'o[inner9]', 'unwrap removes the inner wrapper';
is S.new.m, 'sub1', "the subclass's own method is not wrapped";
