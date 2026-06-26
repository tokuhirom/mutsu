use Test;

plan 7;

# A `:D`/`:U`/`:_` definedness smiley must attach to a TYPE (`Int:D $x`), never
# trail a parameter after whitespace. A trailing smiley is X::Parameter::InvalidType
# ("Invalid typename '<X>'"), not an invocant `:` marker.
# See roast/S32-exceptions/misc.t (old-issue-tracker #3092).
throws-like 'sub foo ($bar :D) { 1 }', X::Parameter::InvalidType,
    'untyped param + trailing :D smiley is X::Parameter::InvalidType';
throws-like 'sub foo (Int $bar :D) { 1 }', X::Parameter::InvalidType,
    'typed param + trailing :D smiley is X::Parameter::InvalidType';
throws-like 'sub foo ($bar :U) { 1 }', X::Parameter::InvalidType,
    'trailing :U smiley is X::Parameter::InvalidType';

# A proper type-attached smiley still works.
{
    sub g(Int:D $x) { $x * 2 }
    is g(5), 10, 'Int:D $x type-attached smiley works';
}

# An invocant `:` marker (method) is unaffected.
{
    my class C { method m($self: $x) { $x + 1 } }
    is C.new.m(4), 5, 'method invocant `$self:` marker still parses';
}

# Plain and multi-param signatures are unaffected.
{
    sub h($a, $b) { $a + $b }
    is h(2, 3), 5, 'plain multi-param signature still works';
}
{
    sub n(:$x) { $x }
    is n(x => 9), 9, 'named parameter `:$x` still works';
}
