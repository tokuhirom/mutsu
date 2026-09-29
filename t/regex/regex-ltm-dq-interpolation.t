use Test;

# ADR-0022 Slice 5: a runtime `$var` / `{$x}` inside a double-quoted regex
# literal ends the declarative prefix in alternation LTM, so the first
# alternative wins instead of the longest literal.

plan 5;

my $x = "ab";
is ("abc" ~~ / "$x" | a /).Str, "a", '"$x" is not declarative';
is ("abc" ~~ / "{$x}" | a /).Str, "a", '"{$x}" is not declarative';
is ("abc" ~~ / "${x}" | a /).Str, "a", '"${x}" is not declarative';
is ("abc" ~~ / "ab" | a /).Str, "ab", 'plain literal stays declarative';
constant $c = "ab";
is ("abc" ~~ / "$c" | a /).Str, "ab", 'constant stays declarative';
