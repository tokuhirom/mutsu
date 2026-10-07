use Test;

# From the RedFactory distribution: `Metamodel::ClassHOW.new.new_type: :name(..)`.
plan 4;

my $how = Metamodel::ClassHOW.new;
ok $how.defined, 'Metamodel::ClassHOW.new is a defined HOW instance';
my \T = Metamodel::ClassHOW.new.new_type: :name("FooFactory");
is T.^name, 'FooFactory', 'new_type on a HOW instance mints a named type';
T.^add_attribute: Attribute.new: :name<$!x>, :package(T), :type(Str), :has_accessor;
T.^compose;
lives-ok { T.new }, 'the minted type composes with an added attribute and instantiates';
my \E = Metamodel::EnumHOW.new;
ok E.defined, 'Metamodel::EnumHOW.new does not die';
