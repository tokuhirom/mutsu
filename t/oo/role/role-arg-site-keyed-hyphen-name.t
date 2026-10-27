use Test;

# Found by Red 0.2.7 (t/100-single-relationship-create): a second type minted
# under an already used name is keyed `name\0<digits>`; when the name has a
# hyphen, passing that type as a role argument was re-parsed as source and
# died with a parse error.

plan 3;

role Holder[\T] { method held { T } }

my \first  = Metamodel::ClassHOW.new_type(:name<typed-child>);
first.^compose;
my \second = Metamodel::ClassHOW.new_type(:name<typed-child>);
second.^compose;

my \host = Metamodel::ClassHOW.new_type(:name<host>);
host.^add_role(Holder[second]);
host.^compose;

ok host.held =:= second, 'the second same-named hyphenated type is the role argument';
ok !(host.held =:= first), 'it is not confused with the first one';
is host.held.^name, 'typed-child', 'its name is the written name';
