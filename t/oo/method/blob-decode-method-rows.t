use Test;

plan 16;

# Blob.decode / Buf.decode (ADR-11276 §8.3): one decoder behind the rows and the cascade.
my $u = "héllo wörld".encode;
is $u.decode, 'héllo wörld', 'decode defaults to utf-8';
is $u.decode('utf-8'), 'héllo wörld', 'decode with an encoding name';
is $u.decode('UTF-8'), 'héllo wörld', 'the encoding name is case-insensitive';
is $u.decode('latin-1').chars, 13, 'decode as latin-1 reads one char per byte';
is Buf.new(104, 105).decode, 'hi', 'Buf.decode';
is Blob.new(104, 105).decode, 'hi', 'Blob.decode';
is utf8.new(98, 117).decode, 'bu', 'utf8.decode';
is Buf.new.decode, '', 'decode of an empty buffer';

is "hé".encode('utf-16').decode('utf-16'), 'hé', 'utf-16 round trip';
is "é".encode('latin-1').decode('latin-1'), 'é', 'latin-1 round trip';
is "abc".encode('ascii').decode('ascii'), 'abc', 'ascii round trip';
throws-like { Blob.new(200).decode('ascii') }, Exception, 'a byte above 127 is not ascii';
throws-like { Blob.new(0xff, 104).decode }, Exception, 'malformed utf-8 dies';

my $b = Buf.new(65, 66);
$b.push(67);
is $b.decode, 'ABC', 'decode sees a pushed byte';
is $b.subbuf(1).decode, 'BC', 'decode of a subbuf';
is $u.decode.encode.decode, 'héllo wörld', 'encode then decode';

done-testing;
