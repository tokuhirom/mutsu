my enum Lengths is export(:Lengths) ( px => 1, pt => 2 );
role Foo::RU[\dimension, \units] {
    method dimension{dimension}
    method units {units}
    sub postfix:<px>(Numeric $v) is rw is export(:px) { $v but Foo::RU[Lengths, 'px'] };
}
role Foo::RU {
    sub dimension(\units) is export(:dimension) { 'routine' }
}
