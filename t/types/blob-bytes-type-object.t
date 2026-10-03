use v6;
use Test;

# `Blob.bytes` is defined on instances only. On a Buf/Blob type object it
# used to count the bytes of the type's name, so `$buf // Buf[uint8]`
# (HTTP::Tiny's test handle at end of input) reached OpenSSL's `BIO_write`
# through NativeCall as a 12-byte buffer and crashed the process.

plan 6;

throws-like { Buf[uint8].bytes }, X::Parameter::InvalidConcreteness,
    'Buf[uint8] type object';
throws-like { Blob.bytes }, X::Parameter::InvalidConcreteness, 'Blob type object';
throws-like { buf8.bytes }, X::Parameter::InvalidConcreteness, 'buf8 type object';

is Buf[uint8].new(1, 2, 3).bytes, 3, 'instance: one byte per uint8';
is buf16.new(1, 2).bytes, 4, 'instance: two bytes per uint16';
is Blob.new.bytes, 0, 'empty instance';
