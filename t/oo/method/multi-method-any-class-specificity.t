use lib 't/lib';
use Test;
use Issue8566::Mod;

# A `multi method` candidate typed with a user-defined class must be ranked
# more specific than a sibling candidate typed `Any` — a strict subtype
# always wins over its ancestor. Previously the method-multi type-distance
# ranker special-cased `value_type_name` == "Any" (the generic answer it
# gives for every `Instance`, since it does not know the concrete class) as
# an automatic distance-0 match, so an `Any` candidate always tied (or, once
# that tie broke via an unrelated bug, silently WON) against the genuinely
# narrower class-typed candidate. https://github.com/tokuhirom/mutsu/issues/8566
plan 2;

# Same compilation unit: `Foo` is a strict subtype of `Any`, in the same
# file as `User`.
{
    class Foo {
        has $.name;
    }

    class Usr {
        multi method show(Any $a) {
            die "fell through to Any";
        }
        multi method show(Foo $a) {
            return "Foo: " ~ $a.name;
        }
    }

    is Usr.new.show(Foo.new(name => 'hi')), 'Foo: hi',
        'same-file: a class-typed multi method candidate beats Any';
}

# Across a `use`d module boundary: `Foo` is imported into this file's
# lexical scope as a bare alias for `Issue8566::Mod::Foo`.
{
    class Usr2 {
        multi method show(Any $a) {
            die "fell through to Any";
        }
        multi method show(Foo $a) {
            return "Foo: " ~ $a.name;
        }
    }

    is Usr2.new.show(Foo.new(name => 'hi')), 'Foo: hi',
        'cross-module: an imported class-typed multi method candidate beats Any';
}
