use Test;
use NativeCall;

plan 3;

class SIn is repr('CStruct') {
    has int16  $.fam  is rw;
    has uint16 $.port is rw;
    has uint32 $.addr is rw;
}
class SAny is repr('CUnion') {
    HAS SIn $.in;
}
class AI is repr('CStruct') {
    has int32  $.ai_flags;
    has int32  $.ai_family;
    has int32  $.ai_socktype;
    has int32  $.ai_protocol;
    has uint32 $.ai_addrlen;
    has SIn    $.ai_addr is rw;
    has Str    $.cn;
    has AI     $.ai_next is rw;
}
class AIU is repr('CStruct') {
    has int32  $.ai_flags;
    has int32  $.ai_family;
    has int32  $.ai_socktype;
    has int32  $.ai_protocol;
    has uint32 $.ai_addrlen;
    has SAny   $.ai_addr is rw;
    has Str    $.cn;
    has AIU    $.ai_next is rw;
}

sub gai(Str, Str, AI, Pointer is rw --> int32) is native is symbol('getaddrinfo') {*}
sub gai2(Str, Str, AIU, Pointer is rw --> int32) is native is symbol('getaddrinfo') {*}
sub native-socket(int32, int32, int32 --> int32) is native is symbol('socket') {*}
sub native-close(int32 --> int32) is native is symbol('close') {*}
sub bind-struct(int32, SIn is rw, int32 --> int32) is native is symbol('bind') {*}
sub bind-union(int32, SAny is rw, int32 --> int32) is native is symbol('bind') {*}
sub bind-plain(int32, SIn, int32 --> int32) is native is symbol('bind') {*}

my AI $h .= new(:ai_socktype(1));
my Pointer $res .= new;
gai("127.0.0.1", "0", $h, $res);
my $ai = nativecast(AI, $res);

my $fd = native-socket(2, 1, 0);
is bind-struct($fd, $ai.ai_addr, 16), 0, 'is rw CStruct param passes the struct address';
native-close($fd);

my AIU $h2 .= new(:ai_socktype(1));
my Pointer $res2 .= new;
gai2("127.0.0.1", "0", $h2, $res2);
my $ai2 = nativecast(AIU, $res2);
my $fd2 = native-socket(2, 1, 0);
is bind-union($fd2, $ai2.ai_addr, 16), 0, 'is rw CUnion param passes the union address';
native-close($fd2);

my $fd3 = native-socket(2, 1, 0);
is bind-plain($fd3, $ai.ai_addr, 16), 0, 'a plain CStruct param passes the struct address';
native-close($fd3);
