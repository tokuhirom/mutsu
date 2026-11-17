use Test;

plan 12;

nok Blob.new(1, 2, 3) ~~ Blob[uint8], 'plain Blob is not Blob[uint8]';
nok Blob.new ~~ Blob[uint16], 'empty Blob has no inferred element type';
nok Buf.new(1) ~~ Buf[uint8], 'plain Buf is not Buf[uint8]';
ok Buf.new(1) ~~ Blob[uint8], 'plain Buf does Blob[uint8]';
ok Blob[uint8].new(1) ~~ Blob[uint8], 'sized Blob matches its type';
ok Buf[uint8].new(1) ~~ Buf[uint8], 'sized Buf matches its type';
ok Buf[uint8].new(1) ~~ Blob[uint8], 'sized Buf does matching Blob';
nok Buf[uint16].new(1) ~~ Blob[uint8], 'different element widths do not match';
ok 'a'.encode ~~ Blob[uint8], 'UTF-8 encoding retains its sized Blob type';
ok Buf.new(1) ~~ blob8, 'blob8 accepts a plain Buf';
throws-like { my Blob[uint8] $b = Blob.new(1) }, X::TypeCheck::Assignment,
    'assignment rejects a plain Blob';
sub takes-sized-blob(Blob[uint8] $b) { }
throws-like { takes-sized-blob(Blob.new(1)) }, X::TypeCheck::Binding,
    'binding rejects a plain Blob';
