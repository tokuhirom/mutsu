use Test;

# `.^get_attribute_for_usage($name)` returns the type's own Attribute of that
# full name, and dies for anything else, including a parent's attribute.

plan 7;

class P { has $.p }
class C is P { has $.a; has @.list; has %.h }

isa-ok C.^get_attribute_for_usage('$!a'), Attribute, 'a scalar attribute';
is C.^get_attribute_for_usage('$!a').name, '$!a', 'it is the named attribute';
is C.^get_attribute_for_usage('@!list').name, '@!list', 'an array attribute';
is C.^get_attribute_for_usage('%!h').name, '%!h', 'a hash attribute';

dies-ok { C.^get_attribute_for_usage('$!zz') }, 'an undeclared attribute dies';
dies-ok { C.^get_attribute_for_usage('$!p') }, "a parent's attribute is not the type's own";

# The ClassX::StrictConstructor idiom: probe each sigil, catching the miss.
sub has-attr($type, $attr) {
    for <$! @! %!> -> $prefix {
        CATCH { default { next } }
        $type.^get_attribute_for_usage($prefix ~ $attr);
        return True;
    }
    False
}
is-deeply <a list h zz>.map({ has-attr(C, $_) }).List, (True, True, True, False),
    'probing every sigil finds exactly the declared names';
