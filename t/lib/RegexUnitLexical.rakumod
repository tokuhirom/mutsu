# Fixture for t/regex/regex-interp-escaped-our-sub-lexical.t: a compunit
# file-scope lexical with a kebab-case name, interpolated by an exported sub.
unit module RegexUnitLexical;
my Str $current-decimal = '.';
our sub whole-part(Str:D $s) is export {
    $s ~~ / ^ (\d+) [ $current-decimal (\d ** 1..2) ]? $ / ?? +$0 !! Nil
}

my Str $symbol = 'GBP';
our sub money(Int $n) is export { $symbol ~ $n }
