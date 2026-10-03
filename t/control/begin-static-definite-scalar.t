use Test;

# A `:D`-typed scalar declared before a BEGIN-time effect (a BEGIN block or a
# `use`) is split into its static half and a run-time assignment. The static
# half holds the nominal type object, which the `:D` constraint rejects, so it
# must not be type-checked. Found via the Usage::Utils distribution, whose
# module starts with `my Bool:D $debug = False;` followed by `use` statements.

plan 9;

{
    my Int:D $x = 3;
    my $seen;
    BEGIN { $seen = $x.raku }
    is $seen, 'Int', 'a BEGIN sees the type object of a :D scalar';
    is $x, 3, 'the run-time initializer still applies';
    throws-like { $x = Nil }, X::TypeCheck::Assignment,
        'the :D constraint still holds after the split';
    throws-like { $x = 'str' }, X::TypeCheck::Assignment,
        'the nominal type still holds after the split';
}

{
    my Bool:D $debug = False;
    use Test;
    is $debug, False, 'a `use` after a :D declaration leaves it intact';
}

{
    my Int:D $x = 3;
    my $inner;
    BEGIN { $x = 5; $inner = $x }
    is $inner, 5, 'a BEGIN can assign a defined value to the static container';
    is $x, 3, 'the run-time initializer overwrites the BEGIN-time value';
}

{
    sub f { my Str:D $s = 'ok'; BEGIN { 1 }; $s }
    is f(), 'ok', 'a :D scalar in a routine body before a BEGIN';
}

throws-like 'my Int:D $x = Int; BEGIN { 1 }', X::TypeCheck::Assignment,
    'a type-object initializer is still rejected at run time';
