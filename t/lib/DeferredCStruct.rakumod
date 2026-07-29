unit class DeferredCStruct;
use DeferredCStruct::Arr;
use DeferredCStruct::Native;

has $!binds;
has int $!count;

submethod BUILD() {
    $!count = 3;
    $!binds = Linear[NB].new($!count);
    $!binds[$_].a = 10 + $_ for ^$!count;
}

method stride()     { $!binds.stride }
method elem-type()  { $!binds.elem-type }
method arr-name()   { $!binds.^name }
method read-a($i)   { $!binds[$i].a }
method write-b($i, $v) { $!binds[$i].b = $v; True }
method read-b($i)   { $!binds[$i].b }
