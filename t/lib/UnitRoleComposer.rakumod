unit role UnitRoleComposer;

# `use` is compile-time, so this must load before the `also does` below resolves.
use UnitRoleBase;
also does UnitRoleBase;

method own() { 'own' }
