use Test;

plan 5;

# An unknown encoding name throws, like `encode` (#12341).
try { Buf.new(1).decode('nonesuch') }
isa-ok $!, X::AdHoc, 'decode with an unknown encoding throws X::AdHoc';
like $!.message, /"Unknown string encoding: 'nonesuch'"/, 'message names the encoding';

# buf16 / blob16 default to utf-8 over their bytes; utf16 stays UTF-16.
is buf16.new(0x68, 0x69).decode.raku, '"h\0i\0"', 'buf16.decode defaults to utf-8';
is blob16.new(0x68, 0x69).decode.raku, '"h\0i\0"', 'blob16.decode defaults to utf-8';
is utf16.new(0x68, 0x69).decode, 'hi', 'utf16.decode is UTF-16';
