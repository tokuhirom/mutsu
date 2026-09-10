use v6;
use Test;

# A bare `$name` in a regex, where `$name` is declared by an in-regex
# `:my $name …` declaration, interpolates that lexical's string value as a
# literal *at match time* (Raku semantics). mutsu previously pre-substituted
# `$name` from the outer `env` before matching, so a regex-local `:my` var — whose
# value is only known while matching — was invisible and the interpolation became
# a never-match. Now it lowers to a match-time `VarInterp` atom reading the value
# from the running capture store.

plan 5;

# Plain regex.
ok ("xxy" ~~ / :my $p = 'xx'; $p 'y' /).defined,
    'plain regex: :my $p = "xx"; $p matches the literal "xx"';
nok ("zzy" ~~ / :my $q = 'xx'; $q 'y' /).defined,
    'plain regex: the interpolation is the :my value, not a wildcard';

# Grammar token.
grammar G { token TOP { :my $v = 'ab'; $v 'c' } }
ok G.parse("abc").defined, 'grammar token: :my $v interpolates in a token';
nok G.parse("xyc").defined, 'grammar token: only the :my value matches';

# An empty :my value interpolates as the empty string (zero-width), so the
# following atom matches from the same position.
grammar E { token TOP { :my $w = ''; $w <[\d]>+ } }
ok E.parse("5").defined, 'grammar token: empty :my value is a zero-width match';
