use Test;

# `Blob`'s and `Buf`'s `allocate`, `encoding` and `chars` are rows
# (ADR-11276 §9.163).

plan 16;

is Buf.allocate(3).raku, 'Buf.new(0,0,0)', 'Buf.allocate';
is Blob.allocate(2, 1).raku, 'Blob.new(1,1)', 'Blob.allocate with a fill value';
is Buf.allocate(4, (1, 2)).raku, 'Buf.new(1,2,1,2)', 'a list fill is cycled';
is Buf.allocate(3, Blob.new(7, 8)).raku, 'Buf.new(7,8,7)', 'a buffer fill is cycled';
is buf8.allocate(2).raku, 'Buf[uint8].new(0,0)', 'an alias class allocates its own type';
throws-like { Buf.allocate(2, "x") }, X::TypeCheck, 'a Str fill does not bind';
throws-like { Buf.new(1).allocate(2) }, X::Multi::NoMatch, 'an instance has no allocate';

is utf8.encoding, 'utf-8', 'utf8.encoding';
is utf16.encoding, 'utf-16', 'utf16.encoding';
is utf8.new(65).encoding, 'utf-8', 'an encoding buffer instance names its encoding';
is Blob.encoding.raku, 'Any', 'Blob.encoding is Any';
is Buf.new(65).encoding.raku, 'Any', 'a plain buffer has no encoding';

throws-like { Buf.new(65).chars }, X::Buf::AsStr, 'Buf.chars throws X::Buf::AsStr';
throws-like { Blob.new(65).chars }, X::Buf::AsStr, 'Blob.chars throws X::Buf::AsStr';
throws-like { utf8.new(65).chars }, X::Buf::AsStr, 'utf8.chars throws X::Buf::AsStr';
throws-like { Buf.new(65).chop }, X::Method::NotFound, 'a buffer is not Cool';
