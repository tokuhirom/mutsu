use v6;
use Test;

plan 9;

# A `token`/`rule`/`method` multi-dispatch variant may be named with a bare
# identifier after the colon (`token gap:spacer {...}`), not just the familiar
# `:sym<literal>` form. Both spellings register a candidate under the proto;
# the bare one differs only in that it binds no `<sym>` literal.

grammar Gaps {
    token TOP { <gap>+ }
    proto token gap {*}
    token gap:spacer { \s }
    token gap:comment { '#' \N* }
}

ok Gaps.parse("  ##comment"), 'bare-adverb token variants dispatch under their proto';
nok Gaps.parse("xyz"), 'a non-matching input still fails';

# A bare-adverb method is *registered*, not run at class-composition time.
my $ran = 0;
class Registers {
    method bar:common ($x) { $ran = $x }
}
is $ran, 0, 'bare-adverb method body does not execute at composition time';
ok Registers.^can('bar:common'), 'bare-adverb method is registered under its full name';

# The whole point of the spelling: a grammar's action class names its methods
# to match the grammar's own bare-adverb token variants.
grammar Kv {
    token TOP { <pair>+ % \n }
    proto token pair {*}
    token pair:num { $<k>=[\w+] '=' $<v>=[\d+] }
    token pair:str { $<k>=[\w+] '=' '"' $<v>=[<-["]>*] '"' }
}
class KvActions {
    method pair:num ($/) { make ~$<k> => +$<v> }
    method pair:str ($/) { make ~$<k> => ~$<v> }
    method TOP ($/) { make $<pair>.map(*.made).Hash }
}
my $m = Kv.parse("a=1\nb=\"two\"", actions => KvActions);
ok $m, 'grammar with bare-adverb variants parses';
is-deeply $m.made, {a => 1, b => 'two'}, 'action methods dispatch by bare-adverb name';

# `Array[Str:D]` — the colon belongs to the parameterisation's inner type, so
# the outer type has no smiley and needs no initializer.
{
    my Array[Str:D] @nested;
    is @nested.elems, 0, 'my Array[Str:D] @x declares without an initializer';
}
{
    my Str:D @flat = 'x', 'y';
    is-deeply @flat.List, ('x', 'y'), 'a real :D smiley on the outer type still applies';
}

# An actually-invalid smiley is still rejected.
throws-like 'my Int:foo $x;', X::InvalidTypeSmiley,
    'an unknown smiley is still a compile-time error';

done-testing;
