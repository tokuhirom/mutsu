use Test;

# "\r\n" is one grapheme that a character class tests as `\n`, negated
# classes included: `<-[\n]>` must not match it. From PDF::Grammar's
# `token literal:sym<regular> {<-literal-delimiter>+}` stopping at a CRLF.

plan 8;

nok "\r\n" ~~ /^<-[\n]>$/, '<-[\n]> does not match CRLF';
ok  "\r\n" ~~ /^<[\n]>$/, '<[\n]> matches CRLF';
nok "\r\n" ~~ /^<[\r]>$/, '<[\r]> does not match CRLF';
ok  "\r\n" ~~ /^<-[\r]>$/, '<-[\r]> matches CRLF';
ok  "\r\n" ~~ /^<-[a]>$/, '<-[a]> matches CRLF';
nok "\r\n" ~~ /^<-[\s]>$/, '<-[\s]> does not match CRLF';

grammar G {
    token d { <[ ( ) \n ]> }
    token TOP { <-d>+ }
}
is ~G.subparse("hi\r\nagain"), 'hi', '<-subrule>+ stops at a CRLF';
is "a\r\nb".comb(/<-[\n]>+/).join('|'), 'a|b', 'comb with a negated class splits on CRLF';
