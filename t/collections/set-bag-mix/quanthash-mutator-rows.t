use Test;

plan 8;

# SetHash.set / .unset take exactly one positional (a list is iterated one level).
my $s = SetHash.new(1, 2, 3);
try { $s.unset(1, 2); CATCH { default { is .^name, 'X::AdHoc', 'unset with two args throws'; like .message, /'Too many positionals passed; expected 2 arguments but got 3'/, 'unset too-many message' } } }
is $s.elems, 3, 'a rejected unset removes nothing';

try { $s.set(); CATCH { default { is .^name, 'X::AdHoc', 'set() throws'; like .message, /'Too few positionals passed; expected 2 arguments but got 1'/, 'set too-few message' } } }

$s.unset((1, 2));
is $s.elems, 1, 'unset with one list removes each element';
$s.set((4, 5));
is $s.elems, 3, 'set with one list adds each element';
$s.set(9);
ok $s{9}, 'set with a single key';
