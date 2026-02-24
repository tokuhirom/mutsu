use Test;

plan 2;

my \a = 0, (1, 2);
is a[1].gist, '(1 2)', 'sigilless declaration preserves nested comma-list RHS';

my \b = 10, 20, 30;
is b[1], 20, 'sigilless declaration keeps full RHS list';
