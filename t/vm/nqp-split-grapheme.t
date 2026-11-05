use v6;
use Test;
use nqp;

# From CSS::Grammar t/regex-tokens.t: Test.rakumod escapes newlines in a test
# description with nqp::split("\n", ...). MoarVM splits a grapheme array, so
# the "\n" of a "\r\n" grapheme is not a separator.

plan 6;

is nqp::elems(nqp::split("\n", "a\r\n")), 1, 'no split inside a CRLF grapheme';
is nqp::elems(nqp::split("\n", "a\nb")), 2, 'plain LF still splits';
is nqp::elems(nqp::split("\n", "a\nb\r\nc")), 2, 'only the bare LF splits';
is nqp::join("|", nqp::split("\r", "a\r\n")), "a\r\n", 'CR inside CRLF is not a separator either';
is nqp::join("|", nqp::split(",", "a,b,,c,")), "a|b||c|", 'trailing empty field kept';
is nqp::elems(nqp::split("\n", "")), 0, 'empty string gives no fields';
