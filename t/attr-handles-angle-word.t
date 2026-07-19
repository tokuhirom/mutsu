use v6;
use Test;

# `handles<...>` (an angle-word delegation list with no space before the `<`)
# on an attribute must parse. Previously the parser required whitespace after
# the `handles` keyword, so `has $.x handles<a b>` failed to parse and the whole
# `has` declaration fell back to expression parsing ("Variable $.x used where no
# 'self' is available" / "Unsupported reduction operator"). Surfaced by
# SQL::Abstract (`has Row:D $.values is required handles<elements>;`).

plan 5;

# 1. Delegation via a no-space angle-word list works.
{
    class Inner { method greet { "hi" } }
    class C {
        has Inner $.inner handles<greet> = Inner.new;
    }
    is C.new.greet, "hi", 'handles<name> (no space) delegates';
}

# 2. Multiple delegated methods.
{
    class Inner { method a { "A" }; method b { "B" } }
    class C {
        has Inner $.inner handles<a b> = Inner.new;
    }
    is C.new.a ~ C.new.b, "AB", 'handles<a b> delegates multiple methods';
}

# 3. The spaced form still works (unchanged).
{
    class Inner { method greet { "yo" } }
    class C {
        has Inner $.inner handles <greet> = Inner.new;
    }
    is C.new.greet, "yo", 'handles <name> (with space) still works';
}

# 4. A typed :D attribute with a trailing trait and a no-space handles list —
#    the exact SQL::Abstract shape.
{
    class Row { method elements { <a b c> } }
    class C {
        has Row:D $.values is required handles<elements>;
    }
    is C.new(:values(Row.new)).elements.join, "abc",
        'typed :D attr with is required + handles<elements>';
}

# 5. A single delegated method.
{
    class Inner { method only { 42 } }
    class C { has $.inner handles<only> = Inner.new; }
    is C.new.only, 42, 'handles<only> single method';
}
