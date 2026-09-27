use NestedBlockMethodUnitRole;
unit class NestedBlockMethodUnit does NestedBlockMethodUnitRole;

has @!record = 1, 2, 3;

do { # hide this sub
    proto sub unrecord(Mu) is raw {*}
    multi sub unrecord(Mu \value) { value * 10 }

    method unrecord(::?CLASS:D: --> List:D) {
        @!record.map(&unrecord).List
    }
}
