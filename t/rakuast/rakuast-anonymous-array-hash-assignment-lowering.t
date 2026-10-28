use Test;

# From Color::Names: `my constant \COLORS is export(:colors) =%= %( ... )`.
# Rakudo reads `=%=` as `=` applied to an anonymous `state` hash, so the
# RakuAST has `VarDeclaration::Anonymous(sigil => "%")` as the assignment
# target. The lowering knew only the anonymous scalar target (`$ = 5`).
plan 8;

is-deeply (my @a = (@ = 1, 2)), [1, 2], 'anonymous array as an assignment target';
is-deeply (my %h = (% = a => 1)), {a => 1}, 'anonymous hash as an assignment target';

my constant \C = %= (a => 1, b => 2);
is C<b>, 2, '%= on a constant initializer';

my constant \COLORS is export(:colors) =%= %( 'x-CSS3' => { rgb => [1, 2, 3], name => 'X' } );
is COLORS<x-CSS3><rgb>.join(','), '1,2,3', '=%= with an export trait';

my constant \L =@= (1, 2, 3);
is-deeply L, [1, 2, 3], '=@= on a constant initializer';

my @rows;
for 1..3 { @rows.push: (@ = $_, $_ * 2).join('+') }
is @rows.join(' '), '1+2 2+4 3+6', 'a fresh anonymous array per evaluation';

@ = 1, 2;
% = a => 1;
pass 'bare statements assigning to anonymous @ and %';

my $ok = ($ = 5);
is $ok, 5, 'the anonymous scalar form still works';

# vim: expandtab shiftwidth=4
