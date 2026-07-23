use Test;

# String coercion of Nil via `.Str` / `.Stringy` warns "Use of Nil in string
# context" and resumes with the empty string (like raku). `.gist` / `.raku`
# render "Nil" and do NOT warn.
#
# Regression: mutsu returned "" from `Nil.Str` with no warning at all.

plan 8;

# `quietly { ... }` swallows the "Use of Nil in string context" warning; assert
# the resumed value is the empty string.
is (quietly { Nil.Str }),     '', 'Nil.Str is the empty string';
is (quietly { Nil.Stringy }), '', 'Nil.Stringy is the empty string';
ok (quietly { Nil.Str }).defined, 'Nil.Str is a defined empty string (not a type object)';
is (quietly { Nil.Str }).chars, 0, 'Nil.Str has zero characters';

# `.gist` / `.raku` render "Nil" and must NOT be diverted to the empty string.
is Nil.gist, 'Nil', 'Nil.gist is "Nil" (unaffected)';
is Nil.raku, 'Nil', 'Nil.raku is "Nil" (unaffected)';

# The warning actually fires: capture it via a CONTROL handler.
{
    my $warned = False;
    {
        Nil.Str;
        CONTROL { when CX::Warn { $warned = True; .resume } }
    }
    ok $warned, 'Nil.Str emits a warning';
}
{
    my $warned = False;
    {
        Nil.gist;
        CONTROL { when CX::Warn { $warned = True; .resume } }
    }
    nok $warned, 'Nil.gist emits no warning';
}
