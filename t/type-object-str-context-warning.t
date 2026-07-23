use Test;

# Using a bare type object in a string context warns
# "Use of uninitialized value of type X in string context." (or, inside string
# interpolation, "...value element of type X...") and resumes with the empty
# string, matching raku. This covers every string-context entry point that a
# type object can reach: the prefix:<~> and infix:<~> operators, `eq`/`lt`/...
# string comparisons, `print`, and interpolation.
#
# A class that defines a user `.Str`/`.Stringy` dispatches it instead of warning,
# even when the invocant is the bare type object.
#
# `.gist` / `.raku` (and `say`, which uses `.gist`) render the type name and do
# NOT warn — they are unaffected.
#
# Regression: mutsu resumed with "" from prefix/infix `~`, `eq`, and
# interpolation with no warning at all (only `print` and the `.Str` method
# warned).

plan 20;

# --- prefix:<~> operator ---
is (quietly ~Int), '', '~Int is the empty string';
{
    my $warned = False;
    { my $x = ~Int; CONTROL { when CX::Warn { $warned = True; .resume } } }
    ok $warned, '~Int emits a warning';
}
{
    my $msg;
    { my $x = ~Int; CONTROL { when CX::Warn { $msg = .message; .resume } } }
    is $msg,
        "Use of uninitialized value of type Int in string context.\n" ~
        "Methods .^name, .raku, .gist, or .say can be used to stringify it to something meaningful.",
        '~Int warning has raku wording';
}

# --- infix:<~> concatenation ---
is (quietly ('a' ~ Int)), 'a', '"a" ~ Int drops the type-object operand';
is (quietly (Str ~ Num)), '', 'both type-object operands drop to empty';
{
    my $count = 0;
    { my $x = Str ~ Num; CONTROL { when CX::Warn { $count++; .resume } } }
    is $count, 2, 'Str ~ Num warns once per type-object operand';
}

# --- string comparison ---
ok (quietly (Int eq '')), 'Int eq "" is true (Int stringifies to empty)';
nok (quietly (Int eq 'x')), 'Int eq "x" is false';
{
    my $warned = False;
    { my $b = Int eq 'x'; CONTROL { when CX::Warn { $warned = True; .resume } } }
    ok $warned, 'Int eq "x" emits a warning';
}

# --- print ---
{
    my $warned = False;
    { print Int; CONTROL { when CX::Warn { $warned = True; .resume } } }
    ok $warned, 'print Int emits a warning';
}

# --- interpolation ---
is (quietly "a{Int}b"), 'ab', 'interpolating a type object yields the empty string';
{
    my Int $y;
    is (quietly "y=$y"), 'y=', 'interpolating an uninitialized typed var yields empty';
}
{
    # The common `$var` form uses raku's "element" wording exactly.
    my Int $y;
    my $msg;
    { my $s = "y=$y"; CONTROL { when CX::Warn { $msg = .message; .resume } } }
    is $msg,
        "Use of uninitialized value element of type Int in string context.\n" ~
        "Methods .^name, .raku, .gist, or .say can be used to stringify it to something meaningful.",
        'variable interpolation uses raku\'s "element" wording';
}
{
    # Block `{...}` interpolation of a type object still warns and resumes with
    # "" (mutsu emits the "element" wording here too; raku uses "of type" for
    # the block form, a cosmetic difference not asserted).
    my $warned = False;
    { my $s = "a{Int}b"; CONTROL { when CX::Warn { $warned = True; .resume } } }
    ok $warned, 'block interpolation of a type object emits a warning';
}

# --- a user .Str / .Stringy dispatches even on the bare type object ---
{
    class WithStr { method Str { 'STR' } }
    is (~WithStr), 'STR', '~TypeObject dispatches a user .Str';
    is ("<" ~ WithStr ~ ">"), '<STR>', 'infix ~ dispatches a user .Str';
    is ("[{WithStr}]"), '[STR]', 'interpolation dispatches a user .Str';
}
{
    class WithStringy { method Stringy { 'STRINGY' } }
    is (~WithStringy), 'STRINGY', '~TypeObject dispatches a user .Stringy';
}

# --- .gist / .raku are unaffected (no warning, render the type name) ---
is Int.gist, '(Int)', 'Int.gist is "(Int)" (unaffected)';
{
    my $warned = False;
    { my $g = Int.gist; CONTROL { when CX::Warn { $warned = True; .resume } } }
    nok $warned, 'Int.gist emits no warning';
}
