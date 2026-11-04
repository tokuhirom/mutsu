use v6;
use Test;

# A wrapper that stores the `callsame` Match in a container (an object
# attribute, an array, a hash) must not make the rule's grammar action run a
# second time (Grammar::Extractor).

plan 3;

grammar NG { token TOP { <word> '!'? }; token word { \w+ } }
class Acts {
    has @.log;
    method TOP($/)  { @!log.push: "TOP" }
    method word($/) { @!log.push: "word" }
}
class H { has $.result is rw }

my $a = Acts.new;
my $h = H.new;
my @st;
my %h;
for NG.^methods.grep: { .WHAT ~~ Regex && .name eq 'word' } -> &rule {
    &rule.wrap: my sub (|c) {
        my Match $result := callsame;
        $h.result = $result;
        @st.push($result);
        %h<r> = $result;
        return $result;
    }
}
NG.parse("hello!", :actions($a));
is $a.log.join(" "), "word TOP", 'action runs once per rule';
is $h.result.Str, "hello", 'stored Match is the word';
is @st.elems, 1, 'wrapper ran once';
