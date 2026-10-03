# ADR-0135 Slice E: a capture group with a scoped `:ignoremark` body, and a
# call of a `:ignoremark` rule, run on the compiled regex engine. The group's
# ends come from the mark-stripped subject (`GroupEnds`) inside the capture's
# own level; the call's ends from the all-ends entry, which runs the rule's
# stripped program. Values are rakudo's.
use Test;

plan 5;

is ~("\"é\"" ~~ /(:ignoremark "e") /)[0], 'é', 'a capture group with a :m body';

my $m = "x\"a\"" ~~ /(:m "\"") ~ "\"" (<-["]>*)/;
is ~$m[0], '"', 'the :m capture inside a goal match';
is ~$m[1], 'a', 'the capture after it';

grammar H {
    token TOP { <q>+ % ',' }
    token q { :ignoremark 'cafe' }
}
is H.parse("cafè,cafe")<q>.map(~*).join('|'), 'cafè|cafe', 'a quantified call of a :m rule';
nok H.parse("café,CAFE"), ':m does not imply :i';
