use v6;
use Test;

# A proto-regex candidate can be spelled with the `:<name>` shorthand, not only
# `:sym<name>`. The two differ only in that `:sym<name>` binds a `<sym>` literal;
# `:<name>` is a bare variant name that must still register under the proto.
# The resolver only recognized `:sym<`, so every `token element:<int> {...}`
# candidate was dropped and `<element>` fell through to a "No such method"
# (YAMLish's `Schema::JSON`). The `<|w>` word-boundary assertion is exercised
# too — it was previously unimplemented (matched nothing).

plan 5;

grammar S {
    proto token element { * }
    token element:<int>  { <[+-]>? [ 0 | <[1..9]> <[0..9]>* ] <|w> { make $/.Str.Int } }
    token element:<word> { <[a..z]>+ <|w> { make ~$/ } }
}

is S.parse("42",  :rule<element>).ast, 42,     ':<int> variant matches and makes an Int';
is S.parse("foo", :rule<element>).ast, "foo",  ':<word> variant matches and makes a Str';
nok S.parse("!!", :rule<element>).defined,      'no variant matches punctuation';

# `<|w>` word boundary as a standalone assertion.
ok ("42" ~~ / \d+ <|w> /).defined,  '<|w> matches at a word boundary';
ok ("ab" ~~ / <|w> \w+ <|w> /).defined, '<|w> matches at both boundaries';
