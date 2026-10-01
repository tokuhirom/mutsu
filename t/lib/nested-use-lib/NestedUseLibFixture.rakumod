unit module NestedUseLibFixture;

# Reachable only through the `use lib` written inside a never-called routine
# of t/modules/import-export/use-lib-in-routine-is-begin-time.t.
class X::NestedUseLib::Marker is Exception { }
