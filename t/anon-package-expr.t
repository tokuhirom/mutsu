use Test;

# `anon class { ... }` / `anon role { ... }` / `anon grammar { ... }` as an
# expression must evaluate to the (anonymous) type object. Previously the `anon`
# keyword was not handled in expression context: it parsed as a BareWord and the
# `class`/`role`/`grammar` declaration split off, so `my $c = anon class { ... }`
# stored the string "anon".

plan 8;

# anon class stored in a scalar, then instantiated.
{
    my $c = anon class { has $.bar; };
    is $c.new(:3bar).bar, 3, 'anon class instance attribute';
}

# A method that references the class type via ::?CLASS works on an anon class.
{
    my $c = anon class {
        has $.bar;
        method equal(::?CLASS $foo) { $foo.bar == $.bar }
    };
    ok $c.new(:3bar).equal($c.new(:3bar)), 'anon class ::?CLASS param';
    nok $c.new(:3bar).equal($c.new(:9bar)), 'anon class ::?CLASS param (false case)';
}

# anon class with a method.
{
    my $obj = (anon class { method greet { 'hi' } }).new;
    is $obj.greet, 'hi', 'anon class method call';
}

# The stored value is a type object, not a string.
{
    my $c = anon class { };
    isnt $c.^name, 'Str', 'anon class value is a type object, not Str';
    ok $c.new.defined, 'anon class .new produces a defined instance';
}

# anon grammar parses.
{
    my $g = anon grammar { token TOP { \d+ } };
    ok $g.parse('123'), 'anon grammar .parse succeeds';
}

# anon role evaluates to a role type object (not a string).
{
    my $r = anon role { method tag { 'R' } };
    isnt $r.^name, 'Str', 'anon role value is a type object, not Str';
}
