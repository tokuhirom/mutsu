use v6;
use Test;

# A `<` between the brackets of a character class is a member, not the start of
# another `<...>` assertion: inside a capture group it used to leave the group
# "unmatched" (`/(<-[<]>*)/` -> "Unmatched ( in regex"). Found with Webraku
# (`/ 'id="r">' (<-[<]>*) /`). Identical under raku.

plan 8;

is ("abc<d" ~~ /(<-[<]>*)/)[0].Str, "abc", 'negated class with < in a capture';
is ("abc<d" ~~ /(<-[<]>+)/)[0].Str, "abc", 'with +';
is ("abc<d" ~~ /(<[<]>*)/)[0].Str, "", 'positive class of only <';
is ("<<a" ~~ /(<[<]>+)/)[0].Str, "<<", 'matches the < members';
is ("abc<d" ~~ /(<[<a]>)/)[0].Str, "a", '< and a letter';
is ("xa<by" ~~ /(<[a<b]>+)/)[0].Str, "a<b", '< in the middle of a class';
is ("abc<d>e" ~~ /(<-[<>]>*)/)[0].Str, "abc", '< and > as members';
my $body = '<script id="r" type="application/json">[1,2]</script>';
is ($body ~~ / 'type="application/json">' (<-[<]>*) /)[0].Str, '[1,2]', 'the Webraku shape';
