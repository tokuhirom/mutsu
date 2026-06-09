use Test;

plan 4;

# X::Attribute::Undeclared must expose symbol / package-name / package-kind /
# what accessors and a properly formatted message.
throws-like 'my class Foo { method a() { $!bar } }', X::Attribute::Undeclared,
    symbol       => '$!bar',
    package-name => 'Foo',
    package-kind => 'class',
    what         => 'attribute',
    message      => /'Attribute $!bar not declared in class Foo'/;

throws-like 'my role Bar { method a() { $!qux } }', X::Attribute::Undeclared,
    symbol       => '$!qux',
    package-name => 'Bar',
    package-kind => 'role',
    what         => 'attribute';

# Assignment to an undeclared private attribute reports the same way.
throws-like 'my class Baz { method a() { $!zonk = 1 } }', X::Attribute::Undeclared,
    symbol       => '$!zonk',
    package-name => 'Baz',
    package-kind => 'class';

# Undeclared :$!attr parameter in BUILD is also X::Attribute::Undeclared.
throws-like 'my class Q { submethod BUILD(:$!missing) { } }', X::Attribute::Undeclared,
    symbol       => '$!missing',
    package-name => 'Q',
    package-kind => 'class';
