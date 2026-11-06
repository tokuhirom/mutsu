# Fixture for t/modules/indirect-lookup-finds-program-package-from-module.t
unit module IndirectLookupProbe;
sub look(Str:D $name) is export { ::($name).^name }
sub look-required(Str:D $name) is export {
    try require ::($name);
    ::($name).^name
}
