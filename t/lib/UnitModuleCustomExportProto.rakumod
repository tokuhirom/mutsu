# A custom EXPORT hook and an exported proto can coexist in a unit module.
# The hook is deliberately before `unit module`, matching the shape used by
# LLM::Functions.
sub EXPORT() { Map.new: '&custom-unit-marker' => sub { 'custom' } }

unit module UnitModuleCustomExportProto;

our proto exported-family(|) is export {*}
multi sub exported-family(Str:D $value) { "family:$value" }
