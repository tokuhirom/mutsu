unit module UnitModuleBareIsTrait;

# Fixture for t/modules/import-export/unit-module-bare-is-trait.t (#11349):
# a bare `is foo` on a class or role inside a `unit module` is the named trait
# argument `:foo`, not a parent named `UnitModuleBareIsTrait::foo`.

our @seen;

multi trait_mod:<is>(Mu:U $t, :$marked!) { @seen.push: "marked " ~ $t.^name }

class Plain is marked { }
class WithArg is marked(1) { }
role Roled is marked { }
