# Fixture for t/modules/import-export/unit-role-file-calls-imported-multi.t:
# an exported `proto` with one `multi` candidate (Red::Database's `database`).
unit module ProtoFamilyExporter;
proto open-db(|c) is export { * }
multi sub open-db(Str $type, |c) is export { "db $type" }
