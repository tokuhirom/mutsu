# Fixture for t/modules/import-export/unit-role-file-calls-imported-multi.t:
# hands the importer `&open-db` from a `sub EXPORT`, as `use Red` does.
use ProtoFamilyExporter;
multi EXPORT() { Map.new: '&open-db' => &open-db }
