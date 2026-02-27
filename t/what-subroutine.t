use Test;

plan 2;

my $x = 42;
is (WHAT $x).gist, '(Int)', 'WHAT subroutine form works';
is WHAT($x).gist, '(Int)', 'WHAT subroutine call form works';
