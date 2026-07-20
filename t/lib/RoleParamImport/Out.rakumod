unit role RoleParamImport::Out;
# The `use` sits *inside* the role body, after the `unit role` declaration.
# The imported qualified type is used with a `:D` smiley in a method param /
# return type. This module previously failed to load with
# "Invalid typename 'RoleParamImport::Result:D' in parameter declaration."
use RoleParamImport::Result;
method put(RoleParamImport::Result:D $r --> RoleParamImport::Result:D) {
    $r
}
