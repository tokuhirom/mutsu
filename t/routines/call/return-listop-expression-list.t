use Test;

# `return` in expression position is a listop: all its arguments are the
# returned list (`0 or return 1, 2` returns `(1, 2)`), as for the statement
# form. mutsu returned only the first one (found with #18302).

plan 4;

sub f { 0 or return 1, 2 }
is-deeply f(), (1, 2), 'after `or`';
sub g { 0 || return 3, 4 }
is-deeply g(), (3, 4), 'after `||`';
sub h { 0 or return |(1, 2), 3 }
is-deeply h(), (1, 2, 3), 'a slip flattens into the list';
sub k { 0 or return 5 }
is-deeply k(), 5, 'one argument is the value itself';
