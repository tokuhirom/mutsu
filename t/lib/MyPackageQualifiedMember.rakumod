# A module-level lexical package holding an `our enum`, named qualified
# (`Enums::Status`) from the module's own routines. KeyNub::LicDongle's
# `X::KeyNub::LicDongle.status` has this shape.
my package Enums {
    our enum Status (Ok => 0, Bad => -1);
}

class X::MyPackageQualified is Exception {
    has Int $.code;
    method status() { Enums::Status.^enum_from_value($!code) // Enums::Status }
}

our sub status-type() is export { Enums::Status }
our sub status-name() is export { Enums::Status.^name }
