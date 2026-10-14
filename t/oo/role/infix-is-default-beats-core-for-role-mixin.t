use Test;

# From CSS::Properties (CSS::Units): `multi infix:<+>(U $a, U $b) is default`
# is chosen over the core `(Int:D, Int:D)` candidate for operands that are an
# Int with the role mixed in. The two are equally narrow, so rakudo lets
# `is default` break the tie; without it the call is ambiguous.

plan 5;

role U[\units] { method type { units } }

multi sub infix:<+>(U $v, U $n) is default { 'user +' }
multi sub infix:<->(U $v, U $n) { 'user -' }

my $a = 1 but U['pt'];
my $b = 2 but U['pt'];

is $a + $b, 'user +', 'a role-typed is-default multi wins the tie against the core candidate';
is ($a + 2), 3, 'a plain Int operand still takes the core candidate';
is (5 + 6), 11, 'and so does plain arithmetic';
is $a.type, 'pt', 'the mixin itself is untouched';

sub postfix:<pt>(Numeric $v) { $v but U['pt'] }
is 1pt + 2pt, 'user +', 'through a postfix-built operand';
