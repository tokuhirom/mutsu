use v6;
use Test;

# #8175: a class-level `@`/`%` attribute initializer written as a bare comma
# list, or a parenthesized list, did not behave like a list assignment.
#
# Two independent causes, both fixed here:
#
# 1. The parser stopped at the first comma (`try_dot_twigil_attr` used
#    `expression`, which does not consume a comma list), so `our @.x = 1, 2,
#    3` kept only `1` and silently dropped the rest.
# 2. The evaluated value was stored as-is, skipping
#    `coerce_attr_value_by_sigil` -- the rule every other attribute-store path
#    goes through -- so a parenthesized `(1, 2, 3)` stayed a `List` instead of
#    becoming an `Array` like `has @.x = (1, 2, 3)` already does.
#
# The bracketed spelling (`our @.x = [1, 2, 3]`) and the per-instance `has`
# spelling were already correct and are pinned here too, as a guard against a
# fix that only handles the comma-list/parenthesized cases.

plan 12;

# --- our, comma list and parenthesized list ---
{
    class OurCommaList { our @.x = 1, 2, 3; }
    is OurCommaList.x, [1, 2, 3], 'our @.x = 1, 2, 3 keeps every element';
}

{
    class OurParenList { our @.x = (1, 2, 3); }
    is OurParenList.x, [1, 2, 3], 'our @.x = (1, 2, 3) coerces to an Array';
}

{
    class OurHashCommaList { our %.h = a => 1, b => 2; }
    is OurHashCommaList.h.keys.sort.join(','), 'a,b',
        'our %.h = a => 1, b => 2 keeps every pair';
}

# --- my, same as our for a class-level attribute ---
{
    class MyCommaList { my @.x = 1, 2, 3; }
    is MyCommaList.x, [1, 2, 3], 'my @.x = 1, 2, 3 keeps every element';
}

{
    class MyParenList { my @.x = (1, 2, 3); }
    is MyParenList.x, [1, 2, 3], 'my @.x = (1, 2, 3) coerces to an Array';
}

# --- already-correct spellings, pinned against a partial fix ---
{
    class OurBracket { our @.x = [1, 2, 3]; }
    is OurBracket.x, [1, 2, 3], 'our @.x = [1, 2, 3] is unaffected';
}

{
    class HasCommaList { has @.x = 1, 2, 3; }
    is HasCommaList.new.x, [1, 2, 3], 'has @.x = 1, 2, 3 is unaffected';
}

{
    class HasParenList { has @.x = (1, 2, 3); }
    is HasParenList.new.x, [1, 2, 3], 'has @.x = (1, 2, 3) is unaffected';
}

# --- the := bind and #8150's = copy must still hold on the fixed paths ---
{
    my @source = 1, 2, 3;
    class BoundCommaSource { our @.x := @source; }
    @source.push(4);
    is BoundCommaSource.x, [1, 2, 3, 4], 'our @.x := @c still binds the container';
}

{
    my @source = 1, 2, 3;
    class CopiedCommaSource { our @.x = @source; }
    @source.push(4);
    is CopiedCommaSource.x, [1, 2, 3], 'our @.x = @c still copies (#8150)';
}

# --- a trailing comma is a one-slot list, not a missing element ---
{
    class TrailingComma { our @.x = 5,; }
    is TrailingComma.x, [5], 'our @.x = 5, is a one-element list';
}

# --- a single non-list value still becomes a one-element Array ---
{
    class SingleValue { our @.x = 5; }
    is SingleValue.x, [5], 'our @.x = 5 still becomes a one-element Array';
}
