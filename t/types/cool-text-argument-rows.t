use Test;

plan 14;

# `uniprop`, `uniprops`, `unimatch`, `samecase`, `samemark` and `indent` with
# an argument are `Cool`'s (and `Str`'s) methods, which read the receiver's
# string form. In mutsu they are rows of the built-in method table that a
# `Range`, a `Seq`, an `IO::Path`, a `Match`, an enum and an instance of a
# `Cool` subclass reach (ADR-11276 §9.90); a receiver that is not `Cool` has
# no such method. Every expectation below was checked against Rakudo.
enum E <Ea Eb>;

is (1..3).samecase("AaA"), "1 2 3", 'Range.samecase';
is (1..3).indent(2), "  1 2 3", 'Range.indent';
is-deeply (1, 2).Seq.unimatch("Latin"), False, 'Seq.unimatch';
is Eb.samecase("aB"), "eB", 'an enum reaches samecase';
is-deeply Eb.uniprops("Script"), ("Latin", "Latin"), 'an enum reaches uniprops';
is "a1".IO.samemark("é"), "á1́", 'IO::Path.samemark';
is "  x\n   y".indent(*), "x\n y", 'indent(*) takes the common indentation away';
is-deeply "abc".unimatch("Latin"), True, 'Str.unimatch';
my $m = "xAy" ~~ /A/;
is-deeply $m.uniprop("Script"), "Latin", 'a Match reaches uniprop with a property';

# An allomorph reads its Str part, not its number.
is <65>.uniprop("Script"), "Common", 'IntStr.uniprop reads "6", not codepoint 65';
is-deeply <65>.unimatch("Latin"), False, 'IntStr.unimatch reads its Str part';

throws-like { (a => 1).samecase("A") }, X::Method::NotFound, 'a Pair is not Cool';
throws-like { \(1, 2).indent(2) }, X::Method::NotFound, 'a Capture is not Cool';
throws-like { v1.2.uniprop("Script") }, X::Method::NotFound, 'a Version is not Cool';
