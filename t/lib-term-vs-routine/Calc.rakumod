unit class Calc;
use Foo::RV :Lengths, :&dimension, :px;
has $.u = 'px';
has $.seen is rw;
method run is rw {
    sub FETCH($) { 1 }
    sub STORE($, $v) {
        my $x = Foo::RV.value($v, $!u);
        $!seen = "{$x.units} {$x.dimension.^name}";
    }
    Proxy.new: :&FETCH, :&STORE
}
