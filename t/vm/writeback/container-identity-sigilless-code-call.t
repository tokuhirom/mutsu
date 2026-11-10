use Test;

plan 6;

# `&[=:=]` called through a code object with sigilless bindings of distinct
# element containers compares containers, not the values they hold.
my ($x, $y) = 3, 3;
my $it = [$x, $y].iterator;
my \first = $it.pull-one;
my \nekst = $it.pull-one;
my &w = &[=:=];

is (first =:= nekst), False, 'operator form: two element containers differ';
is &w(first, nekst), False, 'code object call: sigilless element bindings differ';
is &w(first, first), True, 'a binding is identical to itself';
my &as = {$_};
my \tf = &as(first);
my \tn = &as(nekst);
is &w(tf, tn), False, 'bindings of two block results differ';

# The second operand is a call result (`{$_}` hands back the argument's container).
sub f(Iterator:D \it, :&as = {$_}, :&with = &[===]) {
    my \a = it.pull-one;
    my \t = &as(a);
    my \n = it.pull-one;
    &with(t, &as(n))
}
is f([$x, $y].iterator, :with(&[=:=])), False, 'sigilless binding against a call result';
is f([$x, $x].iterator, :with(&[=:=])), False, 'two element containers of one value differ';

# vim: expandtab shiftwidth=4
