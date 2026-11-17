use Test;

# `Str.encode`/`Cool.encode` with an encoding, a normalization form and
# `:replacement`, and `Blob.decode`/`Buf.decode`, are method-table rows
# (ADR-11276); the VM's separate encode/decode entry is gone. Every receiver
# form below goes through the rows, a literal and a variable alike.

plan 16;

is-deeply "abc".encode("utf-16").list, (97, 98, 99), 'literal .encode("utf-16")';
my $s = "héllo";
is $s.encode("utf-8").bytes, 6, 'variable .encode("utf-8")';
is $s.encode("latin-1").list.join(","), "104,233,108,108,111", 'variable .encode("latin-1")';
isa-ok $s.encode("utf-16"), utf16, '.encode("utf-16") is a utf16';
is 42.encode("ascii").decode("ascii"), "42", 'Int receiver (Cool) with an encoding';
is 1.5.encode("utf-8").decode, "1.5", 'Rat receiver (Cool) with an encoding';
is True.encode("utf-8").decode, "True", 'Bool receiver with an encoding';

throws-like { "é".encode("ascii") }, Exception, 'unencodable codepoint dies without :replacement';
is "aé".encode("ascii", :replacement<?>).decode, "a?", ':replacement with a string';
is "aé".encode("ascii", :replacement).decode, "a?", ':replacement with True';
throws-like { "é".encode("ascii", :!replacement) }, Exception, ':!replacement is strict';

is "abc".encode("utf-8", :translate-nl).decode, "abc", ':translate-nl is accepted';

my $b = "héllo".encode;
is $b.decode("utf-8"), "héllo", 'variable .decode("utf-8")';
is Buf.new(104, 233).decode("latin-1"), "hé", 'Buf.decode("latin-1")';
is Buf.new(104, 105).decode("ascii", :replacement<?>), "hi", 'decode accepts :replacement';

class MyStr is Str { }
is MyStr.new(:value<hi>).encode("utf-16").list.join(","), "104,105", 'a Str subclass encodes';
