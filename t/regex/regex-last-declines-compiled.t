# ADR-0135 §8, Slice E, twenty-second part: the pattern shapes the regex
# compiler still declined, and so handed to the tree walk, compile. Each is
# checked against rakudo 2026.07, except the counts past `u32`, where rakudo
# does not finish (it builds the count) and mutsu's answer is the bound's
# meaning: no maximum, and a minimum no subject reaches.
use Test;

plan 14;

{
    my $n = "alpha";
    is ("a" ~~ / <::($n)> /).Str, 'a', 'a symbolic call to a builtin rule';
    grammar Sym { token x { b+ }; token TOP { a <::("x")> } }
    is Sym.parse("abb")<x>.Str, 'bb', 'a symbolic call to a grammar token';
}

{
    grammar ProtoI { proto token t {*}; token t:sym<a> { a }; token TOP { :i <t> } }
    nok ProtoI.parse("A"), 'a proto called under :i does not lend :i to its candidates';
}

is ("a" ~~ / a ** 1..99999999999 /).Str, 'a', 'a maximum past u32 is no bound';
nok "aa" ~~ / a ** 99999999999 /, 'a minimum past u32 is not reached';

is ("aXa,bXb" ~~ / [ (\w) X $0 ]+ % ',' /).Str, 'aXa,bXb',
    'a backreference in a separated quantifier reads its own iteration';

{
    my @l;
    is ("a,a,a" ~~ / :r [ a { @l.push: 1 } ]+? % ',' $ /).Str, 'a,a,a',
        'a frugal separated quantifier under ratchet grows to the anchor';
    is @l.elems, 3, 'its code runs once per iteration entered';
}

is ("äb" ~~ / [:m a { } ] b /).Str, 'äb', 'a [:m …] group with code';
is ("xäy" ~~ / x (:m a) y /)[0].Str, 'ä', 'a (:m …) capture';
is ("xäy" ~~ / x [ c || :m a ] y /).Str, 'xäy', 'an :m branch of a ||';

throws-like ｢"a~b" ~~ / a ~ b /｣, Exception,
    message => /'Unrecognized regex metacharacter ~'/,
    'a stray ~ raises';

# Rakudo reserves `%<name>=` (roast S05-capture/hash.t is not run there);
# mutsu files the spec's hash alias: one key per match, no value.
ok "  a b\tc" ~~ m/%<chars>=( \s+ \S+ )+/, 'a %<name>= hash alias matches';
is $/<chars>.keys.elems, 3, 'it files one key per iteration';
