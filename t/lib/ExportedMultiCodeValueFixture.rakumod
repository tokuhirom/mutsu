unit module ExportedMultiCodeValueFixture;

proto sub compare($, $) is export {*}
multi sub compare(%left, %right) {
    %left.elems == %right.elems && %left<value> eq %right<value>
}
multi sub compare(Any $left, Any $right) is default { False }

sub run() is export {
    my &matcher = &compare;
    my $actual = :value<ok>;
    my $expected = %(value => 'ok');
    matcher($actual, $expected)
}
