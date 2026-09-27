use Test;

plan 6;

# A multi whose name collides with a built-in regex character class (`alpha`,
# `digit`, `xdigit`, ...) must dispatch like any other multi. Before this fix,
# a no-candidate call fell through to the same fallback
# `Grammar.subparse(:rule<xdigit>)` uses to select a built-in character class
# as a start rule, and silently returned a `Regex` (`/<alpha>/`) instead of
# raising `X::Multi::NoMatch` (#9719).
#
# Each call is spread (`|@args`) so the mismatch is only discovered at
# runtime -- a statically-typed mismatched call is (separately) a
# compile-time error in rakudo and out of scope here.

{
    multi sub alpha(Int $x) { "int $x" }
    my @args = "a", "b";
    my $caught;
    { alpha(|@args); CATCH { default { $caught = .^name } } }
    is $caught, 'X::Multi::NoMatch',
        "a multi named 'alpha' raises X::Multi::NoMatch on a no-candidate call";
}

{
    multi sub digit(Int $x) { "int $x" }
    my @args = "a", "b";
    my $caught;
    { digit(|@args); CATCH { default { $caught = .^name } } }
    is $caught, 'X::Multi::NoMatch',
        "a multi named 'digit' raises X::Multi::NoMatch on a no-candidate call";
}

{
    multi sub xdigit(Int $x) { "int $x" }
    my @args = "a", "b";
    my $caught;
    { xdigit(|@args); CATCH { default { $caught = .^name } } }
    is $caught, 'X::Multi::NoMatch',
        "a multi named 'xdigit' raises X::Multi::NoMatch on a no-candidate call";
}

{
    multi sub alnum(Int $x) { "int $x" }
    my @args = "a", "b";
    my $caught;
    { alnum(|@args); CATCH { default { $caught = .^name } } }
    is $caught, 'X::Multi::NoMatch',
        "a multi named 'alnum' raises X::Multi::NoMatch on a no-candidate call";
}

# The legitimate uses of the built-in character-class fallback stay intact:
# a `<alpha>` regex atom, and a grammar start rule with no user token of that
# name.
{
    my $s = "abc123";
    ok $s ~~ /^ <alpha>+ /, 'a <alpha> regex atom still matches the builtin character class';
}

{
    grammar G { token TOP { <alnum> } }
    my $r = G.subparse("a3f", :rule<xdigit>);
    ok $r.defined, 'Grammar.subparse(:rule<xdigit>) still selects the builtin character class';
}
