use Test;

# The reads a grammar action makes of its `$/` are answered from the capture
# tree without materializing the Match (#12917). Each must still give what
# the materialized Match gives.

plan 12;

grammar G {
    token TOP   { <key> '=' <val>+ % ',' <tail>? }
    token key   { \w+ }
    token val   { \d+ }
    token tail  { ';' }
}

class A {
    method TOP($/) {
        make {
            key     => ~$<key>,
            vals    => $<val>.map(~*).join('|'),
            elems   => $<val>.elems,
            missing => $<nope>.raku,
            tail    => $<tail>.raku,
            same    => ($<key> === $<key>),
            str     => ~$/,
            num     => $<val>[1].Numeric,
        }
    }
    method key($/) { make ~$/ ~ '!' }
}

my %r = G.parse('ab=1,22,333', :actions(A)).made;
is %r<key>,     'ab',          '~$<name> reads the capture text';
is %r<vals>,    '1|22|333',    'a quantified capture is a list of Matches';
is %r<elems>,   3,             '... with one element per match';
is %r<missing>, 'Nil',         'an absent capture name is Nil';
is %r<tail>,    'Nil',         'an unmatched optional capture is Nil';
ok %r<same>,                   '$<name> is the same Match on every read';
is %r<str>,     'ab=1,22,333', '~$/ is the matched text';
is %r<num>,     22,            'a capture numifies through its text';

# A grammar's own `Str` still decides `~$/`.
grammar H {
    token TOP { \d+ }
    method Str { 'custom' }
}
class HA { method TOP($/) { make ~$/ } }
is H.parse('42', :actions(HA)).made, 'custom', 'a grammar Str method wins for ~$/';

# A user `prefix:<~>` candidate for Match is still dispatched.
{
    multi prefix:<~>(Match $m) { 'pfx:' ~ $m.Str }
    grammar P { token TOP { \w+ } }
    class PA { method TOP($/) { make ~$/ } }
    is P.parse('hi', :actions(PA)).made, 'pfx:hi', 'a user prefix:<~> for Match is called';
}

# `$/.values` and `.made` of the children read as before.
grammar V { token TOP { <a> <b> } ; token a { x } ; token b { y } }
class VA {
    method TOP($/) { make $/.values.map(*.made).sort.join }
    method a($/)   { make 'A' }
    method b($/)   { make 'B' }
}
is V.parse('xy', :actions(VA)).made, 'AB', '$/.values gives the child Matches';

# A plain regex match reads the same way.
'k=7' ~~ / $<k>=\w '=' $<v>=\d /;
is "{~$<k>}{+$<v>}", 'k7', 'named captures of a regex match';
