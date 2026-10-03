use Test;

plan 5;

# In a signature, `-->` is the return-type arrow even when it follows a
# parameter default with no space: `= True-->Int` is not `True--` then `>`.
sub f(Bool:D :$clone = True-->Int) { $clone ?? 42 !! 0 }
is f(), 42, 'named param default followed by -->';
sub g($x = 5-->Int) { $x }
is g(), 5, 'positional param default followed by -->';
is g(7), 7, 'and an explicit argument still binds';

# A postfix `--` elsewhere is unchanged.
my $y = 5;
is $y--, 5, 'postfix -- still yields the old value';
sub h($n = 3) { my $z = $n; $z--; $z }
is h(), 2, 'postfix -- inside a body with a defaulted param';
