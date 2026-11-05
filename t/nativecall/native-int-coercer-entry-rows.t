use Test;

# ADR-11276 slice 5: the native integer coercers must reach their rows even
# for closed collection shapes and receivers with no settled dispatch shape.
my @methods = <int int8 int16 int32 int64 uint uint8 uint16 uint32 uint64 byte>;

for @methods -> $method {
    subtest $method, {
        my $expected = $method eq 'int8' || $method eq 'uint8' || $method eq 'byte' ?? 44 !! 300;
        is 300."$method"(), $expected, 'integer wraps through its row';
        is '300'."$method"(), $expected, 'string reaches Cool row';
        is (1..3)."$method"(), 3, 'Range reaches Cool row on its count';
        my @array = 1, 2, 3;
        is @array."$method"(), 3, 'Array reads its count';
        my $seq := (1, 2, 3).Seq;
        is $seq."$method"(), 3, 'Seq reads its count';
        my $slip := slip(1, 2, 3);
        is $slip."$method"(), 3, 'Slip reads its count';
        my int @native = 1, 2, 3;
        is @native."$method"(), 3, 'native array reads its count';
        my @shaped[3] = 1, 2, 3;
        is @shaped."$method"(), 3, 'shaped array reads its count';
        is '300'.IO."$method"(), $expected, 'IO::Path reads its string';
        throws-like { Int."$method"() }, X::AdHoc,
            message => /'Can only perform big integer operations on concrete objects'/,
            'Int type object requires a concrete integer';
        is (quietly { Cool."$method"() }), 0, 'Cool type object retains its zero coercion';
        throws-like { (a => 1)."$method"() }, X::Method::NotFound,
            'Pair does not acquire a Cool method';
        class Plain { }
        throws-like { Plain.new."$method"() }, X::Method::NotFound,
            'an Any instance does not acquire a Cool method';
        done-testing;
    }
}

done-testing;
