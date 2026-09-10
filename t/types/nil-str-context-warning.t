use Test;

# Coercing Nil into a string context warns "Use of Nil in string context" and
# resumes with the empty string (like raku). This covers every string-context
# entry point: the `.Str`/`.Stringy` methods, the prefix:<~> and infix:<~>
# operators, `eq`/`lt`/... string comparisons, `print`, and interpolation.
# `.gist` / `.raku` (and `say`, which uses `.gist`) render "Nil" and do NOT warn.
#
# Regression: mutsu returned "" from these paths with no warning at all.

plan 16;

# --- .Str / .Stringy methods ---
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

# --- prefix:<~> operator ---
is (quietly ~Nil), '', '~Nil is the empty string';
{
    my $warned = False;
    { my $x = ~Nil; CONTROL { when CX::Warn { $warned = True; .resume } } }
    ok $warned, '~Nil emits a warning';
}

# --- infix:<~> concatenation ---
is (quietly ('abc' ~ Nil)), 'abc', '"abc" ~ Nil drops the Nil operand';
{
    my $count = 0;
    { my $x = Nil ~ Nil; CONTROL { when CX::Warn { $count++; .resume } } }
    is $count, 2, 'Nil ~ Nil warns once per Nil operand';
}

# --- string comparison ---
ok (quietly (Nil eq '')), 'Nil eq "" is true (Nil stringifies to empty)';

# --- print ---
{
    my $warned = False;
    { print Nil; CONTROL { when CX::Warn { $warned = True; .resume } } }
    ok $warned, 'print Nil emits a warning';
}

# --- interpolation ---
is (quietly "a{Nil}b"), 'ab', 'interpolating Nil yields the empty string';
{
    my $warned = False;
    { my $s = "a{Nil}b"; CONTROL { when CX::Warn { $warned = True; .resume } } }
    ok $warned, 'interpolating Nil emits a warning';
}
