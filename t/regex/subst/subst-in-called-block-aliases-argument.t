use Test;

# From List::MoreUtils (apply.rakutest): `{ s/.../.../ }` called on a variable
# writes through to it, as `{ $_ = ... }` does.
plan 5;

my &b = { s/o/0/ };
my $y = "foo";
b($y);
is $y, "f0o", 's/// in a called block writes the argument';

my &g = { s:g/^ \s+ | \s+ $// };
my $x = " foo ";
g($x);
is $x, "foo", 's:g/// in a called block writes the argument';

my &t = { tr/o/0/ };
my $z = "foo";
t($z);
is $z, "f00", 'tr/// in a called block writes the argument';

sub apply-it(&code, @values) { @values.map( -> $_ is copy { code($_); $_ } ) }
my @list = (" foo ", "     ", "foobar");
is-deeply apply-it({ s:g/^ \s+ | \s+ $// }, @list).List, ("foo", "", "foobar"), 'apply-style map';
is-deeply @list, [" foo ", "     ", "foobar"], 'originals untouched';
