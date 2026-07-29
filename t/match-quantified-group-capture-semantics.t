use v6;
use Test;

# Capture semantics of quantified groups, pinned by DBDish::Pg's
# PgArrayGrammar / `_to-array` (t/36-pg-array.rakutest went from dying at
# test 1 to 46/46 on these).

plan 12;

# --- 1. A CAPTURING group is a capture boundary: named captures inside
# `( ... )*` belong to each group's own Match, NOT the enclosing Match.
{
    my $m = "a,b," ~~ /( $<e>=(\w) \, )*/;
    nok $m.hash<e>:exists, 'capturing group: inner named capture absent from outer hash';
    is $m[0].elems, 2, 'quantified capture group collected per-iteration Matches';
    is ~$m[0][0]<e>, 'a', 'first group Match carries its own named capture';
    is ~$m[0][1]<e>, 'b', 'second group Match carries its own named capture';
}

# --- 2. A NON-capturing group still exposes inner names to the parent as lists.
{
    my $m = "a,b," ~~ /[ $<e>=(\w) \, ]*/;
    ok $m.hash<e>:exists, 'non-capturing group: named capture collected on parent';
    is $m<e>.elems, 2, 'non-capturing group: one entry per iteration';
}

# --- 3. Match.values flattens a quantified positional capture (Capture view).
{
    my $m = "ab" ~~ /(\w)*/;
    is $m.values.elems, 2, '.values flattens the quantified $0 array';
    is $m.keys.elems, 1, '.keys still has the single positional key';
    is-deeply $m.kv.map(~*).list, ('0', 'a', 'b'), '.kv flattens the value after its key';
}

# --- 4. The PgArrayGrammar shape end-to-end.
my grammar G {
    rule TOP     { ^ <array> $ }
    rule array   { '{' ( <element> ','?)* '}' }
    rule element { <array> | <unquoted-string> }
    rule unquoted-string { <-["{},]>+ }
};
{
    my $m = G.parse('{1,2,3}');
    my @texts;
    for $m.<array>.values -> $element {
        @texts.push: ~$element.values[0]<unquoted-string>;
    }
    is-deeply @texts, ['1', '2', '3'],
        'iterating a quantified-group Match .values yields the group Matches';
    is $m.<array>.^name.substr(0, 1), 'G' | 'M',
        'array capture is still a Match-like after the loop';
    ok $m.<array> !~~ Array, 'for <elem>.values does not turn a Match element into an Array';
}
