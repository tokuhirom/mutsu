use v6;
use Test;

plan 10;

# A scalar interpolated into a regex matches literally. Whitespace inside the
# value must NOT be escaped as `\ ` (that is the unspace form, which raku
# rejects outright) -- it has to become a codepoint escape so it still matches.

my $sp = " ";
ok "a b" ~~ /$sp/, 'an interpolated space matches';
ok "x y" ~~ /x$sp/, 'a trailing interpolated space matches';
ok "x y" ~~ /$sp y/, 'a leading interpolated space matches';

my $tab = "\t";
ok "a\tb" ~~ /$tab/, 'an interpolated tab matches';

my $nl = "\n";
ok "a\nb" ~~ /$nl/, 'an interpolated newline matches';

my $cr = "\r";
ok "a\rb" ~~ /$cr/, 'an interpolated carriage return matches';

my $CRLF = "\r\n";
ok "a\r\nb" ~~ /$CRLF/, 'an interpolated CRLF matches';
is-deeply "a\r\nb\r\nc".split(/$CRLF/).List, ("a", "b", "c"),
    'split on an interpolated CRLF';

# Embedded whitespace kept working before; make sure it still does.
my $ab = "a b";
ok "xa by" ~~ /$ab/, 'an interpolated value with inner whitespace matches';

# The value is matched literally, not re-parsed as regex source.
my $dot = ".";
nok "axb" ~~ /$dot/, 'an interpolated "." is a literal, not the any-char atom';
