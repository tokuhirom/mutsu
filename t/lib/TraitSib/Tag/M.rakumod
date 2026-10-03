# The classes are named as siblings of this module's package, not children:
# `TraitSib::Tag::M-X` is not nested in `TraitSib::Tag::M`, where the
# imported `is sib-attr` is recorded.
use TraitSib;
use TraitSib::Attr;

class TraitSib::Tag::M { }

class TraitSib::Tag::M-X is TraitSib::Tag::M {
    has $!value is sib-attr;
    method value { $!value }
}
