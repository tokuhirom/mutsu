# Fixture for t/oo/role/role-does-twice-from-same-named-package.t.
unit role DoesTwice::Rel[&f, :$name];
method rel-name { $name }
