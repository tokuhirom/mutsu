use v6;
use Test;

# A named parameter's alias can chain: `:type(:class($kind))` nests a further
# rename under the alias. The innermost variable (`$kind`) is the one the body
# reads; every level's name (`type`, `class`) is a valid caller-facing key.
# Regression: mutsu only bound/matched one alias level, so a two-or-more-level
# chain threw "Variable 'kind' is not declared" (body) or "Unexpected named
# argument" (deep caller key). See raku-doc Language/signatures.rakudoc.

plan 12;

# --- two-level chain (signatures.rakudoc example) ---
sub two(:color(:$colour), :type(:class($kind))) {
    "$colour $kind"
}
is two(color => "red",   type  => "A"), "red A",   "two-level: outer names";
is two(colour => "green", type => "B"), "green B", "two-level: inner alias + outer";
is two(color => "white", class => "C"), "white C", "two-level: deepest alias key";

# --- five-level chain (signatures.rakudoc example) ---
sub five(:color(:$colour),
         :variety(:style(:sort(:type(:class($kind)))))) {
    "$colour $kind"
}
is five(color => "red",    style   => "A"), "red A",   "five-level: mid key 'style'";
is five(colour => "green", variety => "B"), "green B", "five-level: outermost key 'variety'";
is five(color => "white",  class   => "C"), "white C", "five-level: deepest key 'class'";

# Every level's name is accepted by the caller.
for <variety style sort type class> -> $k {
    my %args = ($k => "X");
    is five(|%args), " X", "five-level: caller key '$k' binds \$kind";
}

# A consumed nested-alias key must not leak into a `*%rest` slurpy.
sub with-rest(:type(:class($kind)), *%rest) {
    "$kind " ~ %rest.keys.sort.join(",")
}
is with-rest(class => "C", other => 1), "C other",
    "nested alias key excluded from *%rest";
