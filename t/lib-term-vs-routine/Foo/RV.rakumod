my enum Lengths is export(:Lengths) ( px => 1, pt => 2 );
my enum Percentages is export(:Percentages) ('%' => 100);
role Foo::RV[\dimension, \units] {
    method dimension{dimension}
    method units {units}
    sub postfix:<px>(Numeric $v) is rw is export(:px) { $v but Foo::RV[Lengths, 'px']  };
}
role Foo::RV {
    my constant %Dimension = %( flat (Lengths, Percentages).map: -> $dim { $dim.keys.map: { $_ => $dim } } );
    sub dimension(\units) is export(:dimension) { %Dimension{units} }
    method value(\v, \units) {
        v ~~ Hash|List
            ?? v does Foo::RV[dimension(units), units]
            !! v but  Foo::RV[dimension(units), units];
    }
}
