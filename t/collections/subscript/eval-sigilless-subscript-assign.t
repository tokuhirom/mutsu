use Test;

# A subscript assignment to a caller's sigilless term inside EVAL must reach
# the bound container (#15388, found by the ADR-15334 slice 1 parity test).
plan 8;

my \sigless = { a => 1 };
EVAL('sigless<a> = 3');
is sigless<a>, 3, 'hash subscript assignment in EVAL reaches the caller term';
is EVAL('sigless<a> = 4; sigless<a>'), 4, 'assignment and read inside one EVAL';
is EVAL('sigless<a>'), 4, 'a later EVAL sees the stored value';

my \arr = [1, 2, 3];
EVAL('arr[1] = 20');
is arr.join(','), '1,20,3', 'positional subscript assignment in EVAL';

sub in-sub(\p) { EVAL('p[0] = "z"'); p }
is in-sub([1, 2]).join(','), 'z,2', 'a sigilless parameter of the calling routine';

my %plain = k => 1;
EVAL('%plain<k> = 2');
is %plain<k>, 2, 'a sigilled hash still works';

my \nested = { outer => { inner => 1 } };
EVAL('nested<outer><inner> = 5');
is nested<outer><inner>, 5, 'a nested subscript chain on a sigilless root';

try { EVAL('nosuch<a> = 1'); CATCH { default { pass 'an unknown name still dies: ' ~ .^name } } }
