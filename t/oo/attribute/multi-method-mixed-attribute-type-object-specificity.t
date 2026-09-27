use v6;
use Test;

plan 1;

# Audio::Hydrogen's XML::Class dispatches positional-element(Attribute, Str)
# between Cool and Mu candidates. A type object is represented as a Package,
# so method dispatch must rank it by its named type's built-in MRO.
role ElementWrapper {
    multi method positional-element(Attribute $attribute, Cool $type, *%_) { 'Cool' }
    multi method positional-element(Attribute $attribute, Mu $type, *%_) { 'Mu' }
}

class Element { }
class Holder { has $.value }
my $node = Element.new does ElementWrapper;

is $node.positional-element(Holder.^attributes[0], Str), 'Cool',
    'the Cool candidate outranks Mu for a mixed invocant and Str type object';
