use Test;

plan 4;

my \a = 0, (1, 2);
is a[1].gist, '(1 2)', 'sigilless declaration preserves nested comma-list RHS';

my \b = 10, 20, 30;
is b[1], 20, 'sigilless declaration keeps full RHS list';

my \c := 42;
is c, 42, 'sigilless declaration accepts bind operator';

my Int \d := 7;
is d, 7, 'typed sigilless declaration accepts bind operator';
