use Test;

# In raku a `Grammar` IS a `Match` subclass (`Grammar.^mro` is
# `(Grammar Match Capture Cool Any Mu)`), and every cursor a parse mints -- the
# top-level result AND every nested capture -- is of the *invoked* grammar's own
# type. mutsu used to hand back a plain `Match` for all of them.
#
# Everything asserted here was measured against real raku first; the file is
# written to pass under `raku t/grammar-parse-result-cursor-type.t` too.

plan 43;

grammar G {
    token TOP { <a> (\d+) }
    token a   { \w }
}

my $m = G.parse("h5");

# --- the headline: the result reports the GRAMMAR's type ---------------------
ok $m.defined, 'a successful parse returns a defined cursor';
is $m.^name, 'G', 'G.parse(...).^name is the grammar name';
is $m.WHAT.^name, 'G', 'G.parse(...).WHAT is (G)';
is ~$m, 'h5', 'the cursor still stringifies to the matched text';

# --- ... while still being a Match --------------------------------------------
ok $m ~~ Match,   'a grammar cursor smartmatches Match';
ok $m ~~ G,       'a grammar cursor smartmatches its own grammar';
ok $m ~~ Capture, 'a grammar cursor smartmatches Capture';
ok $m ~~ Cool,    'a grammar cursor smartmatches Cool';
ok $m ~~ Any,     'a grammar cursor smartmatches Any';
ok $m.isa(Match), 'a grammar cursor .isa(Match)';
ok $m.isa(G),     'a grammar cursor .isa(G)';
isa-ok $m, Match, 'isa-ok $cursor, Match';
isa-ok $m, G,     'isa-ok $cursor, G';

is-deeply G.^mro.map(*.^name).List,
          ('G', 'Grammar', 'Match', 'Capture', 'Cool', 'Any', 'Mu'),
          'G.^mro threads through Grammar -> Match -> Capture -> Cool';

# --- the Match interface still answers on a cursor ---------------------------
is $m.from,   0,    'cursor .from';
is $m.to,     2,    'cursor .to';
is $m.orig,   'h5', 'cursor .orig';
is $m.chars,  2,    'cursor .chars';
is $m.Str,    'h5', 'cursor .Str';
ok $m.Bool,         'cursor .Bool';
is $m.list.elems,   1, 'cursor .list holds the positional capture';
is $m.hash.elems,   1, 'cursor .hash holds the named capture';
is ~$m<a>,   'h',  'cursor named subscript';
is ~$m[0],   '5',  'cursor positional subscript';
is $m.gist.substr(0, 1), "\x[FF62]", 'cursor .gist is the corner-quoted Match gist';

# --- nested cursors are cursors of the SAME grammar --------------------------
is $m<a>.^name, 'G', 'a named sub-capture is a G cursor too';
is $m[0].^name, 'G', 'a positional sub-capture is a G cursor too';
ok $m<a> ~~ Match, 'a nested cursor is still a Match';

# --- inheritance: the INVOKED grammar wins, not the token's owner ------------
grammar H is G {
    token TOP { <a> <b> }
    token b   { \d+ }
}
my $hm = H.parse("h5");
is $hm.^name,      'H', 'H.parse reports H';
is $hm<a>.^name,   'H', 'a token inherited from G still yields an H cursor';
is $hm<b>.^name,   'H', "H's own token yields an H cursor";
ok $hm ~~ Match, 'an inherited-grammar cursor is a Match';

# --- subparse / actions -------------------------------------------------------
is G.subparse("h5xx").^name, 'G', '.subparse also answers a G cursor';

class Act { method TOP($/) { make ~$/ } }
grammar B { token TOP { \w+ } }
my $bm = B.parse("qq", actions => Act);
is $bm.^name, 'B',  'a parse with actions answers a B cursor';
is $bm.made,  'qq', '.made still works on a cursor';
is $bm.ast,   'qq', '.ast still works on a cursor';

# --- a grammar declaring attributes ------------------------------------------
grammar A { has $.invalid; token TOP { \w+ } }
my $am = A.parse("zz");
is $am.^name, 'A', 'a grammar with attributes answers an A cursor';
nok $am.invalid.defined,
    'an unset grammar attribute reads as undefined on the cursor (no dispatch error)';

# --- an ANONYMOUS grammar ----------------------------------------------------
# `my grammar { ... }` registers as `<anon|N>`, which no registry MRO reaches
# `Match` through -- the cursor must still answer as a Match by shape.
{
    my $anon = my grammar { token TOP { \d+ } }.parse("123");
    ok $anon ~~ Match,   'an anonymous grammar cursor smartmatches Match';
    ok $anon.isa(Match), 'an anonymous grammar cursor .isa(Match)';
    is ~$anon, '123',    'an anonymous grammar cursor stringifies to its text';
}

# --- NEGATIVE CONTROLS -------------------------------------------------------
# A plain regex match must stay a bare Match, including one taken inside a
# grammar's own method body.
"x" ~~ /x/;
is $/.^name, 'Match', 'a plain regex match is still (Match)';

grammar C {
    token TOP { \w+ }
    method probe() { return ("abc" ~~ /\w+/) }
}
is C.probe.^name, 'Match',
   'a plain regex match taken inside a grammar method is still (Match)';

done-testing;
