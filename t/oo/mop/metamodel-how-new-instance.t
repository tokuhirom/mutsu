use Test;

# From the RedFactory distribution: `Metamodel::ClassHOW.new.new_type: :name(..)`.
plan 6;

my $how = Metamodel::ClassHOW.new;
ok $how.defined, 'Metamodel::ClassHOW.new is a defined HOW instance';
my \T = Metamodel::ClassHOW.new.new_type: :name("FooFactory");
is T.^name, 'FooFactory', 'new_type on a HOW instance mints a named type';
class MyHOW is Metamodel::ClassHOW { }
my $subclass-how = MyHOW.new;
my \U = $subclass-how.new_type: :name<SubclassFactory>;
is U.^name, 'SubclassFactory', 'new_type on a user HOW subclass instance mints a named type';
is U.HOW.^name, 'MyHOW', 'the minted type keeps the user HOW subclass';
T.^add_attribute: Attribute.new: :name<$!x>, :package(T), :type(Str), :has_accessor;
T.^compose;
lives-ok { T.new }, 'the minted type composes with an added attribute and instantiates';
my \E = Metamodel::EnumHOW.new;
ok E.defined, 'Metamodel::EnumHOW.new does not die';
