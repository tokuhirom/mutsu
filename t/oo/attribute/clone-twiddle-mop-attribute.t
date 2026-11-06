use Test;

plan 5;

# `Attribute.set_value` binds without a type check, so a MOP-built `Str`
# attribute can hold a Block. `.clone` then checks a twiddle against the
# container already in the slot, not against the declared type.
my \T = Metamodel::ClassHOW.new.new_type: :name("TF");
my $a;
T.^add_attribute: $a = Attribute.new: :name<$!t>, :package(T), :type(Str), :has_accessor;
T.^compose;

my %h;
my $o = T.new;
$a.set_value($o, %h<x> = { 1 });
my $b = $o.clone(t => { 42 });
is $b.t.(), 42, 'twiddle over an unchecked slot is not type-checked';
is $o.t.(), 1, 'the original keeps its value';
is %h<x>.(), 1, 'the bound hash element is untouched';

# A slot holding a conforming value still checks the twiddle.
class A { has Str $.t }
my $c = A.new(t => "x");
throws-like { $c.clone(t => { 1 }) }, X::TypeCheck::Assignment, 'a typed slot refuses a Block twiddle';
is $c.clone(t => "y").t, "y", 'a conforming twiddle is accepted';

# vim: expandtab shiftwidth=4
