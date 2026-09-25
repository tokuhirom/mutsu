# Fixture for t/modules/import-export/role-is-export-tag.t: a `unit role`
# exported under a named tag.
unit role UnitRoleExportTagFixture is export(:U);

method who { 'UnitRole' }
