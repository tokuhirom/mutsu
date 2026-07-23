use v6;
use Test;

# An explicit `.Str` / `.Stringy` call on a bare type object stringifies to the
# empty string with rakudo's "Use of uninitialized value of type X in string
# context" warning, matching prefix `~` / interpolation (which already warn).
# `.Stringy`'s default implementation is `self.Str`, so a type object with only a
# user `.Str` stringifies through it; `.Str` never falls back to `.Stringy`.

plan 14;

# Value coercion (warnings quieted; the warning itself is checked below).
is quietly(Int.Str),       "", 'Int.Str -> ""';
is quietly(Str.Str),       "", 'Str.Str -> ""';
is quietly(Numeric.Str),   "", 'Numeric.Str -> ""';
is quietly(Int.Stringy),   "", 'Int.Stringy -> ""';
is quietly(Str.Stringy),   "", 'Str.Stringy -> ""';

role R { }
is quietly(R.Str),         "", 'role type object .Str -> ""';
subset S of Int;
is quietly(S.Str),         "", 'subset type object .Str -> ""';

# `.gist` / `.raku` do NOT warn and keep their representation.
is Int.gist,   "(Int)", '.gist on a type object is unchanged (no warning)';
is Int.raku,   "Int",   '.raku on a type object is unchanged (no warning)';

# A user `.Str` / `.Stringy` dispatches (no warning).
class A { method Str { "AA" } }
is A.Str,      "AA", 'user .Str on a type object dispatches';
is A.Stringy,  "AA", 'user-.Str type object stringifies through default .Stringy';

# A defined value is unaffected.
is "hi".Str,   "hi", 'defined Str value .Str is unaffected';
is 42.Str,     "42", 'defined Int value .Str is unaffected';

# The warning is actually emitted.
{
    my $warned = False;
    my $r;
    {
        CONTROL { when CX::Warn { $warned = True; .resume } }
        $r = Int.Str;
    }
    ok $warned && $r eq "", 'Int.Str emits the string-context warning';
}
