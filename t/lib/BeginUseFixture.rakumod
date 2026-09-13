unit module BeginUseFixture;

# A type whose composed name (`BeginUseFixture::X::BeginUse::Marker`) differs
# from the short name the importer writes, so a stub fabricated for the short
# name is distinguishable from the real type object.
class X::BeginUse::Marker is Exception { }

sub begin-use-probe() is export { 'imported' }
