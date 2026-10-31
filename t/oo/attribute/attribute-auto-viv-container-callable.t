use Test;

# AttrX::Mooish t/010-base: `$inst.fubar.WHAT === $inst.code.WHAT` for `has &.fubar is rw
# is mooish(...)` and a plain `has &.code`. The mooish trait builds the attribute's
# value from `Attribute.auto_viv_container`, which started an untyped `&` attribute
# as `Any` instead of the Callable type object.

plan 6;

class Foo {
    has &.code;
    has $.plain;
    has Int $.typed;
    has @.list;
}

my %by-name = Foo.^attributes.map({ .name => .auto_viv_container });
is %by-name<&!code>.WHAT.raku, 'Callable', 'an untyped & attribute starts as Callable';
is %by-name<$!plain>.WHAT.raku, 'Any', 'an untyped $ attribute is Any';
is %by-name<$!typed>.WHAT.raku, 'Int', 'a typed $ attribute is its type';
is %by-name<@!list>.WHAT.raku, 'Array', 'an @ attribute is an Array';

is Foo.new.code.WHAT.raku, 'Callable', 'the instance agrees';
ok Foo.new.code.WHAT === Foo.^attributes.head.auto_viv_container.WHAT, 'both are the same type object';
