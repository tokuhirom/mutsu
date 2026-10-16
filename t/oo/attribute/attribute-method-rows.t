use Test;

# `Attribute`'s metaobject methods are rows of the method table, found by
# their owner (ADR-11276 §9.121).

plan 22;

class A {
    has Int $.x is rw;
    has $!y = 5;
    has @.z is required;
}

my ($x, $y, $z) = A.^attributes;

is $x.name, '$!x', 'name';
is $x.type.^name, 'Int', 'type';
is $y.type.^name, 'Mu', 'type of an untyped attribute';
ok $x.has_accessor, 'has_accessor of a public attribute';
nok $y.has_accessor, 'has_accessor of a private attribute';
ok $x.rw, 'rw';
nok $x.readonly, 'readonly of an rw attribute';
ok $y.readonly, 'readonly';
ok $z.required, 'required';
is $x.package.^name, 'A', 'package';
is $x.gist, 'Int $!x', 'gist';
is $x.Str, '$!x', 'Str';
is $x.raku, 'Attribute.new', 'raku is Mu.raku';
is Int.^attributes.raku, '(BOOTSTRAPATTR.new,)', 'raku of a bootstrap attribute';

my $o = A.new(x => 3, z => [1]);
is $x.get_value($o), 3, 'get_value';
is $y.get_value($o), 5, 'get_value of a private attribute';
$x.set_value($o, 42);
is $o.x, 42, 'set_value';

my $trait-attr;
multi trait_mod:<is>(Attribute:D $a, :$made-rw!) { $a.set_rw; $trait-attr = $a }
class B { has $.w is made-rw }
ok $trait-attr.rw, 'set_rw from a trait';

ok Attribute.^can('get_value'), '.^can sees get_value';
ok Attribute.^can('name'), '.^can sees name';

throws-like { A.^attributes[0].get_value(A) }, Exception,
    message => /'Cannot look up attributes in a A type object'/,
    'get_value on a type object';

is $x.build.^name, 'Mu', 'build of an attribute with no initializer';
