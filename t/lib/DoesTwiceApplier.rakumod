# Fixture for t/oo/role/role-does-twice-from-same-named-package.t: a role
# whose name ends like the role it mixes in (Red's MetamodelX::Red::Relationship
# mixing Red::Attr::Relationship into attributes).
use DoesTwice::Rel;
unit role DoesTwiceApplier::Rel;
method apply($attr, $name) { $attr does DoesTwice::Rel[{ 1 }, :$name] }
