use Test;

# `R.^parameterize($value)` parameterizes the role by the value itself, as
# `R[$value]` does: the role body sees the value and the name shows its type.
# From Badger's `sub (...) { ... } does SignatureOverload.^parameterize($sig)`.

plan 5;

my role SO[$sig] { method sig { $sig } }

is SO.^parameterize(5).^name, 'SO[Int]', 'name shows the argument type';

my $s = :(Int $a);
my $sub = (sub (Int $a) { $a + 1 } does SO.^parameterize($s));
is $sub(2), 3, 'the mixed-in sub still runs';
ok $sub.sig === $s, 'the role body sees the signature value';
is $sub.^name, 'Sub+{SO[Signature]}', 'mixin name';

my $built = Signature.new(:params(()));
lives-ok { (sub { 1 } does SO.^parameterize($built)) }, 'a Signature.new value parameterizes';
