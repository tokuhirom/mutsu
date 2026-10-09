use Test;

plan 5;

class C { method m(+@a) { 1 } }

# A method frame rebinds `@_` to its own arguments; that must not leak back
# into a caller whose explicit parameter is named `@_` (#12485).
sub f(@_) { C.m(@_); @_ }
my @m = 1, 2, 3, 4;
is-deeply f(@m).List, (1, 2, 3, 4), 'explicit @_ param survives a +@ method call';
is-deeply @m, [1, 2, 3, 4], "the caller's array is not demoted";

sub g(@_) { C.m(1); @_ }
is-deeply g([1, 2, 3]).List, (1, 2, 3), 'explicit @_ survives a +@ method call with other args';

sub h(@_) { my $o = C.new; $o.m(@_); @_ }
is-deeply h([1, 2]).List, (1, 2), 'same through an instance invocant';

sub legacy { C.m(7, 8); @_ }
is-deeply legacy(1, 2).List, (1, 2), 'implicit @_ is unaffected too';
