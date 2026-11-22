use UnitRoleImportMulti::Provider;
unit role UnitRoleImportMulti::ProtoRole;
proto qc($) is export { * }
multi qc(Str $x) { dbz($x) }
