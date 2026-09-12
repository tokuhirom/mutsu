use v6;

# A compunit whose `unit package` is NOT the compunit's own name: the file is
# `UnitPkgExport::Event`, the package it declares into is `UnitPkgExport`. The
# classes therefore register as `UnitPkgExport::Event` and
# `UnitPkgExport::Event::Report`, neither of which sits under the compunit name
# — rakudo still exports the bare names, because `is export` publishes into the
# COMPUNIT's `UNIT::EXPORT`, not into the surrounding package's stash.
#
# This is the exact shape of Test::Async's `Test/Async/Event.rakumod`.
unit package UnitPkgExport;

class Event is export {
    has Int $.id = 0;
    method describe(--> Str:D) { "event " ~ $!id }
}

class Event::Report is Event {
    has Str:D $.message = "";
}

# A sub `is export` from the same compunit already worked; keep it here so a
# regression in either path is distinguishable from a broken fixture.
sub event-tag(--> Str:D) is export { "UnitPkgExport" }
