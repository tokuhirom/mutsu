use Test;

# #14588 (ValueClass's TWEAK freezes attributes): `Attribute.set_value` BINDS
# the value into a scalar attribute, so a container-less value makes the slot
# immutable, while a container keeps it assignable.

plan 6;

class A {
    has $.a;
    method assign($v) { $!a = $v }
}

my $frozen = A.new(a => 1);
my $attr = $frozen.^attributes[0];
$attr.set_value($frozen, $attr.get_value($frozen)<>);
throws-like { $frozen.assign(99) }, X::AdHoc, message => /'Cannot assign to an immutable value'/,
    'a decontainerized value freezes the slot';
is $frozen.a, 1, 'the value is unchanged';

my $plain = A.new;
$plain.^attributes[0].set_value($plain, 5);
throws-like { $plain.assign(6) }, X::AdHoc, message => /'Cannot assign to an immutable value'/,
    'a literal value binds too';
is $plain.a, 5, 'the accessor reads the bound value';

my $other = A.new(a => 2);
lives-ok { $other.assign(3) }, 'another instance stays assignable';
is $other.a, 3, 'and takes the assignment';
