use Test;

# Two textually distinct anonymous classes are two types. Their type objects
# (`.WHAT`) used to lose the `__ANON_CLASS_N__` marker and collapse onto one
# nameless value, so they compared `===` equal and shared one `.WHICH`, and
# `.WHAT.^name` / `.WHAT.gist` showed no name. `.WHAT` of an anonymous class is
# the class itself, named `<anon|N>`. Expected values come from `raku`.

plan 24;

my $a = class { };
my $b = class { };

# The reported identity.
nok $a.WHAT === $b.WHAT, 'two anonymous classes have distinct .WHAT';
nok $a.WHAT eqv $b.WHAT, 'and are not eqv';
isnt $a.WHAT.WHICH.Str, $b.WHAT.WHICH.Str, 'and have distinct .WHICH';
ok $a.WHAT.WHICH.Str.starts-with('<anon|'), '.WHICH of an anonymous class names it';

# What already agreed stays so.
nok $a === $b, 'the two classes are not ===';
nok $a.new === $b.new, 'their instances are not ===';
isnt $a.^name, $b.^name, 'their .^name differ';

# `.WHAT` of the class, of its instances and of itself is one type.
ok $a.WHAT === $a, '.WHAT of an anonymous class is the class itself';
ok $a.WHAT === $a.WHAT, '.WHAT is stable';
ok $a.new.WHAT === $a.WHAT, 'an instance reports its class as .WHAT';
nok $a.new.WHAT === $b.WHAT, 'an instance does not report the other class';
nok $a.new ~~ $b, 'an instance is not of the other class';
ok $a.new ~~ $a, 'an instance is of its own class';
ok $a.new.WHAT.WHICH.Str eq $a.WHAT.WHICH.Str, 'an instance and its class share the .WHAT identity';

# The name is `<anon|N>` wherever it is rendered.
is $a.WHAT.^name, $a.^name, '.WHAT.^name is the class name';
isnt $a.WHAT.^name, $b.WHAT.^name, '.WHAT.^name tells the classes apart';
is $a.WHAT.gist, '(' ~ $a.^name ~ ')', '.WHAT.gist wraps the name in parentheses';
is $a.WHAT.raku, $a.^name, '.WHAT.raku is the name';
is $a.new.WHAT.gist, $a.WHAT.gist, 'an instance .WHAT renders as its class';

# Grammars and roles are named types too.
{
    my $g = grammar { };
    my $r = role { };
    ok $g.WHAT.gist.starts-with('(<anon|'), 'an anonymous grammar .WHAT is named';
    ok $r.WHAT.gist.starts-with('(<anon|'), 'an anonymous role .WHAT is named';
    nok $g.WHAT === $r.WHAT, 'and they differ';
}

# A named class and the builtins are untouched.
is Int.WHAT.gist, '(Int)', 'a builtin type object keeps its name';

class Named { }
is Named.new.WHAT.gist, '(Named)', 'a named class keeps its name';
