use v6;
use Test;

plan 5;

class Foo { }
my \minted = Metamodel::ClassHOW.new_type(:name<AttrPackageMinted>);

my $a = Attribute.new: :name('$!x'), :package(Foo), :type(Int), :has_accessor;
is $a.package.^name, "Foo", "Attribute.package names the type, not its string form";
is $a.package.HOW.^name, "Perl6::Metamodel::ClassHOW", "and is the type object itself";
ok $a.package === Foo, "identical to the type passed in";

my $b = Attribute.new: :name('$!y'), :package(minted), :type(Int), :has_accessor;
is $b.package.^name, "AttrPackageMinted", "a MOP-minted package too";
minted.^compose;
is $b.package.^name, "AttrPackageMinted", "and after it is composed";

