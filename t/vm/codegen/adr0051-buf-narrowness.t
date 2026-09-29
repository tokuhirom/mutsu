use Test;

# #10133 (ADR-0051 P2): multi-dispatch ranks the Buf/Blob family by the
# builtin type catalog's chains, with a sized spelling (`blob8` =
# `Blob[uint8]`) narrower than its base. Every expectation below is
# raku-verified (2026-09-29).

plan 10;

multi f(blob8 $) { 'blob8' }
multi f(Blob $)  { 'Blob' }
is f("a".encode), 'blob8', 'utf8 is narrower as blob8 than as Blob';
is f(blob8.new(1)), 'blob8', 'a blob8 is a blob8';
is f(Blob.new(1)), 'Blob', 'a plain Blob is only a Blob';

multi g(Blob $)       { 'Blob' }
multi g(Stringy $)    { 'Stringy' }
multi g(Positional $) { 'Positional' }
is g(Buf.new(1)), 'Blob', 'Buf ranks Blob first';
is g(buf16.new(1)), 'Blob', 'buf16 ranks Blob first';
is g("x".encode("utf16")), 'Blob', 'utf16 ranks Blob first';

multi k(buf8 $) { 'buf8' }
multi k(Buf $)  { 'Buf' }
is k(buf8.new(1)), 'buf8', 'buf8 is its own spelling';
is k(Buf.new(1)), 'Buf', 'a plain Buf is only a Buf';

multi m(blob16 $) { 'blob16' }
multi m(Blob $)   { 'Blob' }
is m(buf16.new(1)), 'blob16', 'buf16 does blob16';
is m(buf8.new(1)), 'Blob', 'buf8 does not do blob16';
