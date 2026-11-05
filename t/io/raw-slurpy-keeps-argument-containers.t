use Test;
use nqp;
plan 6;

sub f(+@values is raw) { for @values -> Mu $b is raw { $b = 9 } }
my $item = 1;
f($item);
is $item, 9, 'a +@values is raw loop variable aliases the caller variable';

sub g(+@values is raw) { nqp::isrwcont(@values[0]) }
is g($item), 1, 'an element of a +@values is raw slurpy is a writable container';
is nqp::iscont(@(1, 2)[0]) , 0, 'a plain list element is no container';

sub h(*@values is raw) { nqp::isrwcont(@values[0]) }
is h($item), 1, 'an element of a *@values is raw slurpy is a writable container';

sub k(*@values is raw) { @values[0] = 7 }
k($item);
is $item, 7, 'writing a raw slurpy element writes the caller variable';

sub p(+@values) { nqp::isrwcont(@values[0]) }
is p($item), 1, 'a plain +@values slurpy keeps the container too';
