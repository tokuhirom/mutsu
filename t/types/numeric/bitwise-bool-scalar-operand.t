use Test;

plan 4;

# A Bool read out of a hash or array element reaches the integer bitwise
# operators inside a Scalar; it numifies to 1 like a bare `True`, not to 0.
# (Found while fixing #9482: `set(...) »[&infix:<+|>]« set(...)` came out empty.)

my %h = a => True;
is %h<a> +| %h<a>, 1, '+| over a hash element holding True';
my $v = %h<a>;
is $v +& $v, 1, '+& over a scalar copied from it';
is %h<a> +< 1, 2, '+< shifts it as 1';
my @a = True, 3;
is @a[0] +^ @a[1], 2, '+^ over an array element holding True';
