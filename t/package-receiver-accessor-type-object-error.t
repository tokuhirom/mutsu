use Test;

plan 6;

# Calling a public instance-attribute accessor on a bare TYPE OBJECT (no
# `.new`) must raise the same "Cannot look up attributes..." error raku
# raises, not "No such method". `should_bypass_native_fastpath`'s Package
# branch deliberately never routes an accessor-only call to the accessor
# (an instance attribute is meaningless on the type object), so the fast
# native-method cascade used to fall through to a generic "No such method"
# — see t/name-accessor-type-object.t for the pre-existing `.name`-only
# special case this generalizes to any accessor name.

class Foo { has $.x; }

dies-ok { Foo.x }, 'an accessor call on a bare type object dies';
throws-like 'class Quux { has $.x; }; Quux.x', Exception,
    'an accessor call on a bare type object throws an Exception';
throws-like 'class Quuux { has $.x; }; Quuux.x', X::AdHoc,
    :message(/'Cannot look up attributes in a Quuux type object'/),
    'the error message names the type and suggests .new, not "No such method"';

is Foo.new(x => 5).x, 5, 'an accessor call on a real instance is unaffected';

# A private attribute has no public accessor, so this stays "No such method".
throws-like 'class Bar { has $!x; }; Bar.x', X::Method::NotFound,
    'a private (no accessor) attribute name on a type object stays "No such method"';

# A user method of the same name as an accessor still wins (dispatch order
# unaffected by this fix).
class Baz { has $.x; method x { 'overridden' } }
is Baz.x, 'overridden', 'an explicit user method shadowing the accessor still wins on the type object';
