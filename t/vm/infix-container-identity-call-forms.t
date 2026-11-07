use Test;

plan 10;

# `=:=` asks whether two names are one container. The operator form always
# knew; a sigilless parameter and a call through a code object did not.
my ($x, $y) = 3, 3;

sub sigilless(\p, \q) { p =:= q }
is sigilless($x, $y), False, 'sigilless params bound to two variables';
is sigilless($x, $x), True, 'sigilless params bound to one variable';

sub raw($p is raw, $q is raw) { $p =:= $q }
is raw($x, $y), False, 'raw params bound to two variables';
is raw($x, $x), True, 'raw params bound to one variable';

my &viacode = &[=:=];
is viacode($x, $y), False, '&[=:=] held in a variable, two variables';
is viacode($x, $x), True, '&[=:=] held in a variable, one variable';

my &vianame = &infix:<=:=>;
is vianame($x, $y), False, '&infix:<=:=> held in a variable, two variables';
is vianame-same($x), True, '&infix:<=:=> held in a variable, one variable';
sub vianame-same($v) { vianame($v, $v) }

sub apply(&op, \a, \b) { op(a, b) }
is apply(&[=:=], $x, $y), False, 'passed on as a callable, two variables';
is apply(&[=:=], $x, $x), True, 'passed on as a callable, one variable';

done-testing;
