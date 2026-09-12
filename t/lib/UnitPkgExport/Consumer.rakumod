use v6;

# A role whose own body `use`s the module that supplies its method parameter
# types. At the point mutsu validates the role's method signatures the body's
# `use` has not run yet, so the unqualified `Event` cannot be resolved — the
# pre-pass must defer rather than raise X::Parameter::InvalidType.
#
# This is the shape of Test::Async's `Test::Async::Aggregator`.
unit role UnitPkgExport::Consumer;

use UnitPkgExport::Event;

proto method handle(Event, |) {*}

multi method handle(Event:D $ev --> Str:D) { "definite: " ~ $ev.describe }

multi method handle(Event:U \evType, *%c --> Str:D) {
    "type: " ~ evType.new(|%c).describe
}
