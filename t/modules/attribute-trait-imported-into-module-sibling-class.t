use Test;
use lib $*PROGRAM.parent(2).add('lib');

plan 2;

# A namespaced module imports an attribute trait and uses it in a class whose
# name is a sibling of the module's package. The trait resolves even though an
# enclosing package (`TraitSib`) holds unrelated candidates of the same
# `trait_mod:<is>` (HTML::Component's `HTML::Component::Tag::META-CHARSET`).
use TraitSib::Tag::M;
use TraitSib::Attr;

is TraitSib::Tag::M-X.new(value => 7).value, 7,
    'the imported trait ran and re-dispatched to CORE :built';
ok TraitSib::Tag::M-X.^attributes.first(*.name eq '$!value') ~~ TraitSib::Attr,
    'its role mixin is on the attribute';
