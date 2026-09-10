use v6;
use Test;

# Buf/Blob element-type introspection and `.can` on the builtin surface.
# NativeHelpers::Blob branches on `$type.can('allocate')` (a false answer
# sends it down a REPR-poking fallback) and sizes buffers with
# `nativesizeof($buf.of)`.
plan 7;

is Buf.of.^name, 'uint8', 'Buf.of is uint8';
is Buf.new(1, 2).of.^name, 'uint8', 'a Buf instance answers .of too';
is buf16.new(1).of.^name, 'uint16', 'buf16.of is uint16';
is Blob.of.^name, 'uint8', 'Blob.of is uint8';

ok Buf.can('allocate'), 'Buf.can("allocate")';
ok Buf.new(1).can('push'), 'a Buf instance can("push")';
nok Buf.can('no-such-method-xyz'), 'unknown method still answers false';

done-testing;
