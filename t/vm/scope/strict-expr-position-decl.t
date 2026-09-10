use v6;
use strict;
use Test;

# Expression-position declarations under `use strict`.
# Pin for the Humming-Bird t/03 blocker: `ok my $x = $resp.encode, 'desc'`
# died with "X::Undeclared: Variable '$x' is not declared" because the
# expression-position `my` compiled to a plain SetGlobal that the strict
# checker could not tell apart from an undeclared write.

plan 7;

ok my $scalar = 7, 'my $x = ... inside listop args declares';
is $scalar, 7, 'the declared scalar is usable afterwards';

ok (my @arr = 1, 2), 'parenthesized my @a = ... declares';
is-deeply @arr, [1, 2], 'the declared array is usable afterwards';

ok (my %h = a => 1), 'parenthesized my %h = ... declares';
is %h<a>, 1, 'the declared hash is usable afterwards';

ok (my Str $typed = "s"), 'typed expression-position scalar declares';

done-testing;
