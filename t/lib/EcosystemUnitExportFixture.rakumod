unit module EcosystemUnitExportActual;

multi sub choose(Int $value) is export { "int:$value" }
multi sub choose(Str $value) is export { "str:$value" }

our &short-choose is export(:short) = -> $value { "short:$value" };
