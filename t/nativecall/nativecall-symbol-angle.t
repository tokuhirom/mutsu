use Test;
use NativeCall;

plan 1;

sub c_getpid_angle(--> int32) is native('c') is symbol<getpid> { * }

ok c_getpid_angle() > 0, 'angle-bracket symbol trait arguments reach NativeCall';
