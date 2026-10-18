use Test;

# `Mu.emit` is a row (ADR-11276 §9.163): `emit self`, delivered where the
# `emit` routine delivers, for every receiver; a class's own `emit` and a
# `Supplier`'s win.

plan 8;

my @got;
supply { 5.emit; "x".emit; Int.emit; (1, 2).emit }.tap: { @got.push: $_ };
is-deeply @got, [5, "x", Int, (1, 2)], 'a value, a type object and a list emit themselves';

class C { has $.v }
my $obj;
supply { C.new(v => 1).emit }.tap: { $obj = $_ };
is $obj.v, 1, 'an object emits itself';

class D { method emit { "own" } }
is D.new.emit, 'own', "a class's own emit wins";

my $seen;
my $supplier = Supplier.new;
$supplier.Supply.tap: { $seen = $_ };
$supplier.emit(42);
is $seen, 42, "Supplier.emit is the supplier's own";

my $control;
{ 7.emit; CONTROL { default { $control = .^name } } }
is $control, 'CX::Emit', 'outside a supply it is a CX::Emit control exception';

sub e($x) { $x.emit }
my @sub;
supply { e(1); emit 2 }.tap: { @sub.push: $_ };
is-deeply @sub, [1, 2], 'the method form emits into the dynamically enclosing supply';

ok Mu.^can('emit'), 'Mu.^can("emit")';

my @react;
react { whenever supply { 3.emit } { @react.push: $_ } }
is-deeply @react, [3], 'a whenever sees the method form';
