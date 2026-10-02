use v6;
use Test;

# The LTM measurement of a proto's candidates and of a `|`'s branches drops a
# thread whose first leaf cannot match the character in front of it, without
# asking the matcher (#10710). That is only right if the guard admits every
# character the leaf could match: each subject below puts a character at the
# start of the run that a too-narrow guard would wrongly reject.

plan 23;

grammar G {
    token TOP { <t> }
    proto token t {*}
    token t:sym<digit>  { \d+ }
    token t:sym<alpha>  { <[a..z]>+ }
    token t:sym<nl>     { \n }
    token t:sym<space>  { \s+ }
    token t:sym<acute>  { 'é' \w* }
    token t:sym<kha>    { <[क्ष]> }
    token t:sym<neg>    { <-[a..z0..9\s]>+ }
}

class Acts {
    method TOP($/) { make $<t>.made }
    method t:sym<digit>($/)  { make 'digit' }
    method t:sym<alpha>($/)  { make 'alpha' }
    method t:sym<space>($/)  { make 'space' }
    method t:sym<acute>($/)  { make 'acute' }
    method t:sym<nl>($/)     { make 'nl' }
    method t:sym<kha>($/)    { make 'kha' }
    method t:sym<neg>($/)    { make 'neg' }
}

sub which(Str $s) { (G.parse($s, :actions(Acts)) andthen .made) // 'no match' }

is which('123'), 'digit', 'an ASCII digit';
is which("\x[661]\x[662]"), 'digit', 'a non-ASCII digit, which \d admits and a bitmap of ASCII does not';
is which('abc'), 'alpha', 'an ASCII letter';
is which(' '), 'space', 'a space';
is which("\x[A0]"), 'space', 'a no-break space is \s, which a bitmap of ASCII does not say';
is which('éa'), 'acute', 'a non-ASCII literal';
is which("\n"), 'nl', 'a newline';
is which("\r\n"), 'nl', 'CR LF is one newline';
is which('क्ष'), 'kha', 'a class entry that is one grapheme of several characters';
is which('!!'), 'neg', 'a negated class admits ASCII outside its set';
is which('é'), 'acute', 'the literal wins over the negated class on a non-ASCII start';
is which(''), 'no match', 'the end of the subject';

# The same for the branches of a `|`.
grammar H {
    token TOP { [ <d> | <a> | <acute> | <kha> ] }
    token d { \d+ }
    token a { <[a..z]>+ }
    token acute { 'é' \w* }
    token kha { <[क्ष]> }
}
class HActs {
    method TOP($/) { make ($<d> // $<a> // $<acute> // $<kha>).Str.chars ~ ':' ~ ($<d> ?? 'd' !! $<a> ?? 'a' !! $<acute> ?? 'acute' !! 'kha') }
}
sub alt(Str $s) { (H.parse($s, :actions(HActs)) andthen .made) // 'no match' }

is alt('12'), '2:d', 'a | branch: a digit';
is alt("\x[661]"), '1:d', 'a | branch: a non-ASCII digit';
is alt('ab'), '2:a', 'a | branch: a letter';
is alt('éa'), '2:acute', 'a | branch: a non-ASCII literal';
is alt('क्ष'), '1:kha', 'a | branch: a multi-character grapheme class entry';
is alt(''), 'no match', 'a | branch: the end of the subject';

# A guard is built from the first leaf only: a branch that goes on past it
# still measures what it measured.
grammar I {
    token TOP { <t> }
    proto token t {*}
    token t:sym<ab>  { 'ab' 'X' }
    token t:sym<abc> { 'abc' {} 'Y' }
    token t:sym<a>   { 'a' }
}
class IActs {
    method TOP($/) { make $<t>.made }
    method t:sym<ab>($/)  { make 'ab' }
    method t:sym<abc>($/) { make 'abc' }
    method t:sym<a>($/)   { make 'a' }
}
is (I.subparse('abX', :actions(IActs)) andthen .made) // 'no match', 'ab', 'a later leaf is still tried';
is (I.subparse('abcY', :actions(IActs)) andthen .made) // 'no match', 'abc', 'a fate after a guarded leaf still ranks its candidate';
is (I.subparse('abcZ', :actions(IActs)) andthen .made) // 'no match', 'a', 'a candidate that fails falls back to the next';
is (I.subparse('b', :actions(IActs)) andthen .made) // 'no match', 'no match', 'no candidate can start here';
is (I.subparse('a', :actions(IActs)) andthen .made) // 'no match', 'a', 'the shortest candidate alone';
