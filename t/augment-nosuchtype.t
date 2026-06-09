use Test;

plan 5;

# Augmenting a non-existent type is X::Augment::NoSuchType, carrying the
# package-kind and the (possibly qualified) package name.

throws-like 'use MONKEY-TYPING; augment class NoSuchClass { }',
    X::Augment::NoSuchType, package-kind => 'class', package => 'NoSuchClass';

throws-like 'use MONKEY-TYPING; augment class No::Such::Class { }',
    X::Augment::NoSuchType, package => 'No::Such::Class';

throws-like 'use MONKEY-TYPING; augment class NoSuchClass { }',
    X::Augment::NoSuchType, message => /:s does not exist/;

# Augmenting an existing user class still works.
lives-ok {
    EVAL 'class HasC { method a { 1 } }; use MONKEY-TYPING; augment class HasC { method b { 2 } }'
}, 'augmenting an existing class lives';

# Augmenting a builtin type still works.
lives-ok {
    EVAL 'use MONKEY-TYPING; augment class Int { method my-triple { self * 3 } }; 5.my-triple'
}, 'augmenting a builtin type lives';
