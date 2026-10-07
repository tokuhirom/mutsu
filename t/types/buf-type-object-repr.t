use Test;

# `buf8`, `blob16`, ... are aliases of the parameterized roles (`Buf[uint8]`),
# so their type objects are `Uninstantiable`; `utf8`/`utf16` are classes over a
# `VMArray`. NativeCall's `validnctype` accepts a `utf8` parameter by that
# REPR.
#
# Every expectation was verified against Rakudo.

plan 14;

is Buf.REPR, 'Uninstantiable', 'Buf';
is Blob.REPR, 'Uninstantiable', 'Blob';
is buf8.REPR, 'Uninstantiable', 'buf8';
is blob8.REPR, 'Uninstantiable', 'blob8';
is buf16.REPR, 'Uninstantiable', 'buf16';
is blob32.REPR, 'Uninstantiable', 'blob32';
is Buf[uint8].REPR, 'Uninstantiable', 'Buf[uint8]';
is utf8.REPR, 'VMArray', 'utf8';
is utf16.REPR, 'VMArray', 'utf16';

is Buf.new.REPR, 'VMArray', 'a Buf instance';
is buf8.new.REPR, 'VMArray', 'a buf8 instance';
is utf8.new.REPR, 'VMArray', 'a utf8 instance';

use NativeCall;
sub takes-utf8(utf8 $b) is native('c', v6) is symbol('puts') { * }
lives-ok { NativeCall::check_routine_sanity(&takes-utf8) }, 'a utf8 parameter is an accepted NativeCall type';
sub takes-buf8(buf8 $b) is native('c', v6) is symbol('puts') { * }
lives-ok { NativeCall::check_routine_sanity(&takes-buf8) }, 'so is a buf8 one';
