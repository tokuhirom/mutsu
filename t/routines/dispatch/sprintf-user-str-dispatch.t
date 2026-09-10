use Test;
plan 8;

# sprintf %s dispatches a user-defined .Str method (was rendering the .gist).
{
    class S1 { method Str { "CUSTOM" } }
    is sprintf("%s", S1.new), "CUSTOM", '%s dispatches user .Str';
}

# %s with width/flags still routes through user .Str.
{
    class S2 { method Str { "ab" } }
    is sprintf("%5s", S2.new), "   ab", '%s with width uses user .Str';
    is sprintf("%-5s|", S2.new), "ab   |", '%s left-justified uses user .Str';
}

# When a class defines BOTH, %s uses .Str (not .Stringy).
{
    class GB { method Str { "viaStr" }; method Stringy { "viaStringy" } }
    is sprintf("%s", GB.new), "viaStr", '%s uses .Str, not .Stringy';
}

# Multiple %s args each dispatch.
{
    class A { method Str { "A" } }
    class B { method Str { "B" } }
    is sprintf("%s-%s", A.new, B.new), "A-B", 'multiple %s args dispatch';
}

# Positional %s ($) dispatches.
{
    class P { method Str { "P" } }
    is sprintf('%1$s%1$s', P.new), "PP", 'positional %s dispatches';
}

# The captured-outer write inside .Str propagates back to the caller's slot
# (writeback coherence — the surrounding sprintf op drains it).
{
    my $calls = 0;
    class C { method Str { $calls++; "x" } }
    my $c = C.new;
    sprintf("%s", $c);
    sprintf("%s", $c);
    ok $calls >= 2, 'user .Str captured-outer write propagates across sprintf calls';
}

# %d is unaffected by the %s coercion (only %s args are pre-stringified).
{
    is sprintf("%d", 42), "42", '%d numeric formatting unaffected';
}
