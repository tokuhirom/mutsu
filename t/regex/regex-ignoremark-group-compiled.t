# A scoped `[:m …]` group and a whole-pattern `:m` asked for every end (`:ex`)
# run on the compiled regex engine (ADR-0135): the body's program runs over the
# mark-stripped subject and its ends are mapped back, as the walk does. The
# compiled engine used to decline any pattern holding a `[:m …]` group
# (`ignoremark`). Values verified against rakudo.
use Test;

plan 9;

is ~("café" ~~ /caf[:m e]/), 'café', 'a scoped :m group at the end';
is ~("cafè au lait" ~~ /caf [:m e] \s/), 'cafè ', 'an atom after the group';
is ~("Ünïcödé" ~~ /:m Unicode/), 'Ünïcödé', 'a whole-pattern :m';
is ("Ünïcödé" ~~ m:m:ex/U.+/).elems, 6, 'a whole-pattern :m asked for every end';
is ~("xÀÀy" ~~ /x [:m A+] y/), 'xÀÀy', 'a quantifier inside the group';
nok "xÀÀy" ~~ /x [:m A+?] A y/, 'the atom after the group is not under :m';
is ~("naïve" ~~ /:r na [:m i] ve/), 'naïve', 'a ratcheted group';
my $m = "résumé" ~~ /r [:m e] s (u) m [:m e]/;
is ~$m, 'résumé', 'two groups around a capture';
is ~$m[0], 'u', 'the capture between them';
