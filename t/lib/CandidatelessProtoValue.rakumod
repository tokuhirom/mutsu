# Fixture for t/modules/import-export/export-candidateless-proto-value.t
# (#15371): a `my proto` with no candidates yet, kept as a value past its
# module's load, the way CompUnit::Util's `push-unit-multi` stores one.
unit module CandidatelessProtoValue;
our $proto = (my proto bar(Numeric) {*});
$proto.add_dispatchee(sub (Int $) { 'bar-int' });
