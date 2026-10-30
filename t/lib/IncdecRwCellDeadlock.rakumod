use v6.d;
use NativeCall;
unit class IncdecRwCellDeadlock;

method put(Num() $v) {
    my $nb = CArray[num64].new;
    $nb[0] = $v;
}

method decode(Buf $buf) {
    my $pos = 0;
    return self!doc($buf, $pos);
}

method !doc(Buf $buf, $pos is rw) {
    my $k = self!cstr($buf, $pos);
    $k
}

method !cstr(Buf $buf, $pos is rw) {
    $pos++;
    'k'
}
