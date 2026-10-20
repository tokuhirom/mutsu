use v6;
use Test;

plan 7;

# A user subclass of `Attribute` is a metaobject: `.new(:name, :type,
# :package)` records the facts and the `Attribute` methods answer them, next
# to the subclass's own attributes and roles (#12907, PDF::COS::Tie's
# `my class CosOfAttr is Attribute does COSAttrHOW {}`).

my role H { has $.cos is rw }
my class CA is Attribute does H {}

my $a = CA.new(:name<@!x>, :type(Int), :package<?>);
is $a.name, '@!x', '.name';
is $a.type.^name, 'Int', '.type';
isa-ok $a, Attribute, 'it is an Attribute';
$a.cos = 5;
is $a.cos, 5, 'the role attribute is its own';

class Plain is Attribute {}
my $p = Plain.new(:name<$!y>, :type(Str), :package<?>);
is $p.name, '$!y', 'a plain subclass answers .name';
is $p.type.^name, 'Str', 'and .type';

class WithName is Attribute { method name { 'mine' } }
is WithName.new(:name<$!z>, :type(Str), :package<?>).name, 'mine',
    'a method of the subclass wins over the Attribute row';
