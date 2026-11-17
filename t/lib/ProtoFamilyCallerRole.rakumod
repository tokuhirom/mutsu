# Fixture for t/modules/import-export/unit-role-file-calls-imported-multi.t:
# a `unit role` file with no CHECK phaser whose own routine calls the multi
# it imports (Red::Driver::Cache's `cache` calling `database`).
use ProtoFamilyExporter;
use Test;
unit role ProtoFamilyCallerRole;
method role-method {}
proto call-open-db($) is export { * }
multi call-open-db(Str $type) { open-db($type) }
