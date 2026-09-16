# Role parameters do not consume same-named attribute traits

An attribute trait such as `is xml-element` is named dispatch, even when a
parameterized role composed by the owning class has an `xml-element` parameter
whose value is a type object.  mutsu had mistaken that role argument for a
trait type and invoked `trait_mod:<is>` positionally, leaving the imported
trait unhandled.

Trait dispatch now checks the class and enclosing class owners for matching
role-parameter bindings before resolving a trait spelling as a type.  This
matches Rakudo for traits on both the consuming class and nested classes.

Pinned by `t/modules/import-export/imported-attribute-trait-role-composition.t`, reduced
from XML::Class. Its remaining deserialization and round-trip gaps are tracked
in #8528.
