use v6;
use Test;

plan 5;

role Col { method hello { "hi" } }

my \model = Metamodel::ClassHOW.new_type: :name<AttrMixinHolder>;
my $attr = Attribute.new: :name('$!data_1'), :package(model), :type(Int), :has_accessor;
$attr does Col;
model.^add_attribute: $attr;

is model.^attributes.grep(Col).elems, 1, ".^attributes keeps the role the attribute was mixed with before compose";
model.^compose;
is model.^attributes.grep(Col).elems, 1, "and after compose";
is model.^attributes[0].hello, "hi", "the mixed-in method is callable on the returned attribute";

my $plain = Attribute.new: :name('$!x'), :package(model), :type(Int), :has_accessor;
my \other = Metamodel::ClassHOW.new_type: :name<AttrPlainHolder>;
other.^add_attribute: $plain;
other.^compose;
is other.^attributes.grep(Col).elems, 0, "an attribute without the role is not matched";
is other.^attributes.elems, 1, "but is still listed";
