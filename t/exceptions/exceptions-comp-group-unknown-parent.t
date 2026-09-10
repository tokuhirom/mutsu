use Test;

plan 11;

# X::Inheritance::UnknownParent exposes .suggestions / .child / .parent and a
# "Did you mean" message when a close type name exists.
{
    try EVAL('my class FooBarBazQuuxNoMatch is TotallyUnknownParentName { }');
    isa-ok $!, X::Inheritance::UnknownParent, 'unknown parent throws X::Inheritance::UnknownParent';
    is +($!.suggestions), 0, 'no suggestions for a strange parent name';
    ok $!.message !~~ /:s Did you mean/, 'no "Did you mean" when there are no suggestions';
}

{
    class Animal { }
    try EVAL('my class Dog is Animaal { }');
    is $!.suggestions.sort, <Animal>, 'close parent name is suggested';
    ok $!.message ~~ /:s Did you mean/, '"Did you mean" appears when a suggestion exists';
}

# `my <unknown-type> $x` is a grouped compile failure: X::Comp::Group whose
# first sorrow is X::Undeclared carrying type-name suggestions.
{
    try EVAL('my cool $a');
    isa-ok $!, X::Comp::Group, 'undeclared type in my-decl throws X::Comp::Group';
    isa-ok $!.sorrows[0], X::Undeclared, 'first sorrow is X::Undeclared';
    is $!.sorrows[0].suggestions.sort, <Bool Cool>, 'cool suggests Bool and Cool';
}

# Levenshtein threshold follows rakudo: a 4-char typo tolerates only 1 edit, so
# a distance-2 user class must NOT be suggested.
{
    class Foo { }
    try EVAL('my cool $a');
    is $!.sorrows[0].suggestions.sort, <Bool Cool>, 'distance-2 class Foo is not suggested for cool';
}

# A longer typo tolerates more edits.
{
    try EVAL('Ecxeption.new("x")');
    isa-ok $!, X::Undeclared::Symbols, 'Ecxeption throws X::Undeclared::Symbols';
    is $!.type_suggestion<Ecxeption>.grep("Exception"), ["Exception"], 'Exception is suggested for Ecxeption';
}
