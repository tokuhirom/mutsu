use v6;
use Test;
use NativeCall;

# Two classes each declare a lexical `is native` sub of the same bare name
# (Net::BGP's TCP::LowLevel has `native-close` in two modules). Binding the
# second routine's `$!do` used to retire the first routine's wrap chain, so
# the first class's calls ran the `{*}` body and returned `*`.

plan 6;

class A {
    sub nc(int32 --> int32) is native is symbol('close') {*}
    method c { nc(99999) }
}
class B {
    sub nc(int32 --> int32) is native is symbol('close') {*}
    method c { nc(99999) }
}

is B.c, -1, 'the later class calls its native sub';
is A.c, -1, 'the earlier class still calls its own native sub';
is A.c, -1, 'and again';

class C {
    sub nd(int32 --> int32) is native is symbol('close') {*}
    method c { nd(99999) }
}
class D {
    sub nd(int32 --> int32) is native is symbol('getppid') {*}
    method c { nd(0) }
}
is C.c, -1, 'distinct symbols: first class keeps its symbol';
cmp-ok D.c, '>', 0, 'distinct symbols: second class gets its own symbol';

is (await start { A.c + B.c }), -2, 'both work from a pool thread';
